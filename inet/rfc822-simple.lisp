(in-package frob)

(export '(find-header parse-mail-rfc822))

(defun find-header (header header-list)
  (find header header-list :key #'header-name))

(defun parse-mail-rfc822 (message)
  (with-input-from-string (stream message)
    (let ((headers (read-headers stream))
	  (body (read-body stream)))
      (values headers body))))

(defun read-line-unfolding (stream)
  (with-output-to-string (string-stream)
    (loop named unfold
	  for char = (read-char stream nil :eof)
	  with previous-char = #\^@
	  until (eql char :eof)
	  do (cond 
	       ((and (char= previous-char #\newline) (char/= char #\space #\tab))
		(unread-char char stream)
		(return-from unfold nil))
	       ((and (char= previous-char #\newline) (or (char= char #\space) (char= char #\tab)))
		(loop for ch = (read-char stream nil :eof)
		      until (or (eql ch :eof) (char/= ch #\space #\tab)))
		(unread-char char stream)
		(write-char #\space string-stream)
		(setq previous-char #\space))
	       (t (unless (char= char #\newline)
		    (write-char char string-stream))
		  (setq previous-char char))))))

(defun read-headers (stream)
  (loop named suck
	with head = nil
	for line = (read-line-unfolding stream)
	do (cond ((or (eql line :eof) (string= line "")) (return-from suck head))
		 (t (push (parse-header line) head)))
	finally (return-from suck head)))

(defun read-body (stream)
  (with-output-to-string (string-stream)
    (loop named suck
	  for char = (read-char stream nil :eof)
	  until (eql char :eof)
	  do (write-char char string-stream))))

(defmethod parse-header ((header string))
  (let* ((fragments (token-pair header :delimiter #\:))
	 (field-name (car fragments))
	 (field-body (string-left-trim '(#\space #\tab) (cdr fragments)))
	 (type (choose-header-type-from-name field-name))
	 (hdr (make-instance type :original header :body field-body :name (intern (string-upcase field-name) (find-package 'keyword)))))
    (parse-header hdr)
    hdr))

(defun choose-header-type-from-name (name) ; blah
  (cond ((string-equal name "from") 'from-header)
	((string-equal name "to") 'to-header)
	(t 'rfc822-header)))

(defclass rfc822-header ()
  ((name :initarg :name :accessor header-name)
   (body :initarg :body :accessor header-body)
   (original :initarg :original :accessor header-original))) ; original header-value before parsing

(defmethod print-object ((hdr rfc822-header) (s stream))
  (format s "#<RFC822 Header: \"~A\">" (header-original hdr)))

(defmethod parse-header ((header rfc822-header))
  (with-slots (original) header
    ))

(defclass from-header (rfc822-header)
  ((addresses)
   (pretty-names)))

;; authentic = "From" ":" mailbox / ( "Sender" ":" mailbox CRLF "From" ":" 1#mailbox )
(defmethod parse-header ((header from-header)))

(defclass date-header (rfc822-header)
  ((day) 
   (month)
   (year)
   (zone)
   (ut)))

(defmethod parse-header ((header date-header)))

(defclass content-type-header (rfc822-header) ()) ; hm

(defmethod parse-header ((header content-type-header)))

(defclass to-header (rfc822-header) ())

(defmethod parse-header ((header to-header)))

(defclass sender-header (rfc822-header) ())

(defmethod parse-header ((header sender-header)))

(defclass received-header (rfc822-header)
  ((from)
   (by)
   (via)
   (with)
   (id)
   (for)
   (time)))

(defmethod parse-header ((header received-header)))





