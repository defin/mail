(in-package mime)

(defun decode-quoted-printable (text)
  (with-input-from-string (in text)
    (with-output-to-string (out)
      (qp-stream-decode in out))))

(defun convert-hex-digit-to-integer (digit-char)
  (let ((digit-code (char-code digit-char)))
    (cond ((<= #.(char-code #\0) digit-code #.(char-code #\9))
	   (- digit-code #.(char-code #\0)))
	  ((<= #.(char-code #\A) digit-code #.(char-code #\F))
	   (+ 10 (- digit-code #.(char-code #\A))))
	  ((<= #.(char-code #\a) digit-code #.(char-code #\f))
	   (+ 10 (- digit-code #.(char-code #\a))))
	  (t (error "~S is not a hex digit." digit-char)))))

(defun hex-digit-char-p (digit-char)
  (let ((digit-code (char-code digit-char)))
    (or (<= #.(char-code #\0) digit-code #.(char-code #\9))
	(<= #.(char-code #\A) digit-code #.(char-code #\F))
	(<= #.(char-code #\a) digit-code #.(char-code #\f)))))

(defun convert-2-hex-digits-to-integer (high low)
  (let ((highval (convert-hex-digit-to-integer high))
	(lowval (convert-hex-digit-to-integer low)))
    (+ (* highval 16) lowval)))

(defun qp-stream-decode (in out)
  (loop for char = (read-char in nil :eof)
	until (eq char :eof)
	if (char= char #\=)
	  do (let ((char2 (read-char in nil :eof)))
	       (unless (eq char2 :eof)
		 (when (or (char= char2 #\Space)
			   (char= char2 #\Tab))
		   ;; accept any number of spaces or tabs after an = while waiting to finish QP expression
		   (loop for char3 = (read-char in nil :eof)
			 until (eq char3 :eof)
			 while (or (char= char3 #\Space)
				   (char= char3 #\Tab))
			 finally (setf char2 char3))))
	       ;; need to test for EOF again cause sucking up spaces and tabs might have run us against EOF
	       (unless (eq char2 :eof)
		 (cond ;; soft-EOL
		   ((char= char2 #\Return)
		    (let ((char3 (read-char in nil :eof)))
		      (unless (eq char3 :eof)
			(when (char= char3 #\Newline)
			  t))))
		   ;; accept a bare newline for soft-EOL for now even though its not in the spec.. later need to make sure all mail is CRLF-terminated and take this out
		   ((char= char2 #\Newline)
		    t)
		   ((hex-digit-char-p char2)
		    (let ((char3 (read-char in nil :eof)))
		      (unless (eq char3 :eof)
			(if (hex-digit-char-p char3)
			    (write-char (code-char (convert-2-hex-digits-to-integer char2 char3)) out)
			    (error "Quoted-Printable malformed: =~C~C does not specify a character code." char2 char3)))))
		   (t (error "Quoted-Printable malformed: =~C does not start a valid construct." char2)))))
	else do (write-char char out)))

#|

(defun qp-encode-character (char)
  (string-upcase (format nil "=~2,'0X" (char-code char))))

(defun qp-accept-literal-p (char)
  (let ((code (char-code char)))
    (or (<= 33 code 60)
	(<= 62 code 126))))


;; Ripped from CL-QPRINT by Bob Marlow

(defun qp-decode (input)
  "INPUT must be a string or a stream. Reads quoted-printable encoding
from INPUT and produces the equivalent decoded string"
  (let ((out-stream (make-string-output-stream))
	(in-stream
	 (typecase input
	   (string (make-string-input-stream input))
	   (stream input))))
    (do ((char (read-char in-stream nil 'eof)
	       (read-char in-stream nil 'eof)))
	((eql char 'eof)
	 (get-output-stream-string out-stream))
      (princ (if (char= char #\=)
		 (let ((char2 (read-char in-stream)))
		   ;; Check for and convert all newlines (LF or CRLF)
		   ;; to nothing. The = indicates a soft line break.
		   (if (member char2 '(#\return #\linefeed)
			       :test #'char=)
		       (let ((char3 (read-char in-stream nil 'eof)))
			 (cond
			   ((eql char3 'eof) "")
			   ((and (char= char3 #\linefeed)
				 (char= char2 #\return)) "")
			   (t char3)))
		       ;; If not a newline the = indicates encoding
		       (code-char (parse-integer
				   (format nil "~C~C"
					   char2
					   (read-char in-stream nil 'eof))
				   :radix 16))))
		 char)
	     out-stream))))


(defun cr-lf (stream)
  "Prints a CRLF sequence to STREAM. RFC 2045 mandates CRLF for newlines"
  (princ #\return stream)
  (princ #\linefeed stream))


(defun qp-encode (input)
  "INPUT must be either a string or a stream. Reads from INPUT and produces
a quoted-printable encoded string"
  (let ((out-stream (make-string-output-stream))
	(in-stream
	 (typecase input
	   (string (make-string-input-stream input))
	   (stream input)))
	(last-line-break 0)
	(ws nil))
    
    (do ((c (read-char in-stream nil 'eof)
	    (read-char in-stream nil 'eof))
	 (position 0 (file-position out-stream)))
	((eql c 'eof)
	 (get-output-stream-string out-stream))

      ;; Put in a soft line break if the line's gotten too long
      (when (>= (- position last-line-break) 74)
	(princ #\= out-stream)
	(cr-lf out-stream)
	(setf last-line-break position))
      
      ;; ws on the end of a line must be encoded
      (when ws
	(if (char= c #\newline)
	    (format out-stream "=~2,'0X" (char-code ws))
	    (princ ws out-stream)))
      
      (cond

	;; Ensure newlines are CR-LF
	((char= c #\newline)
	 (cr-lf out-stream)
	 (setf last-line-break position))

	;; Keep track of whitespace in case of following newlines
	((member c '(#\space #\tab) :test #'char=)
	 (setf ws c))
	
	;; Encode non-printable characters and =
	((or (char< c #\!)
	     (char> c #\~)
	     (char= c #\=))
	 (format out-stream "=~2,'0X" (char-code c)))

	;; Else just print the character.
	(t (princ c out-stream)))

      ;; Keep track of whitespace in case we hit a newline
      (unless (member c '(#\space #\tab) :test #'char=)
	(setf ws nil)))))
|#

