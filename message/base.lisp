(in-package mail-base)

#.(sharpslash:install)

(defclass base-entity-container ()
  ((original-headers
    :accessor entity-original-headers
    :initarg :original-headers
    :type string)
   (original-body
    :accessor entity-original-body
    :type string
    :initarg :original-body
    :initform nil)))

(defparameter *crlf-crlf* (format nil "~%~%"))
(defparameter *crlf* (format nil "~%"))

(defun divide-entity-container (string)
  (if (= 0 (search *crlf* string :test #'string=))
      (let ((headers "")
	    (body (subseq string (length *crlf*))))
	(values headers body))
      (let* ((crlfcrlf (search *crlf-crlf* string :test #'string=))
	     (break (1+ crlfcrlf))
	     (headers (subseq string 0 break))
	     (body (subseq string (1+ break))))
	(values headers body))))

(defconstant +comment-regexp+ (compile-regexp #/"\(.*\)\((.*)\)\(.*\)") 
  "Regexp for matching comments, with submatches for the part before the comment, the comment, and the part after.")

(defun remove-comments-old (string) ; does not handle nested comments
  (loop named chop 
	with input = (subseq string 0)
	with comments = ""
	with done = nil
	until done
	do (multiple-value-bind (success whole before comment after)
	       (match-regexp +comment-regexp+ input)
	     (declare (ignore whole))
	     (if success
		 (progn (setf input (concatenate 'string before after))
			(setf comments (concatenate 'string comments " " comment)))
		 (setf done t)))
	finally (return-from chop (values input (string-trim '(#\Space) comments)))))

(defun remove-comments (string)
  (let ((comment (make-array 100 :element-type 'character :adjustable t :fill-pointer 0))
	(comment-pos 0)
	(comments)
	(nocomment (make-array 100 :element-type 'character :adjustable t :fill-pointer 0)))
    (flet ((push-comment ()
	     (push (cons comment-pos (copy-seq comment)) comments)
	     (setf (fill-pointer comment) 0)))
      (loop with comment-level = 0
	    for i from 0
	    for char across string
	    do (cond ((char= char #\()
		      (incf comment-level)
		      (if (= comment-level 1)
			  (setf comment-pos i)
			  (vector-push-extend char comment)))
		     ((char= char #\))
		      (decf comment-level)
		      (if (= comment-level 0)
			  (push-comment)
			  (vector-push-extend char comment)))
		     (t (if (zerop comment-level)
			    (vector-push-extend char nocomment)
			    (vector-push-extend char comment))))))
    (values (string-trim '(#\Space) nocomment) comments)))

(defun read-line-unfolding (stream)
  (with-output-to-string (string-stream)
    (loop named unfold
	  for char = (read-char stream nil :eof)
	  with previous-char = #\^@
	  until (eql char :eof)
	  do (cond ((and (char= previous-char #\newline) (char/= char #\space #\tab))
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


(defconstant +header-line-regexp+ (compile-regexp #/"\([^:]+\):\(.*\)")
  "Regexp for header lines, with submatches for the field name and field content.")

(defmethod parse-header ((header string))
  (multiple-value-bind (success whole field-name field-content)
      (match-regexp +header-line-regexp+ header)
    (declare (ignore whole))
    (when success
      (let* ((content (string-trim '(#\Space #\Tab) field-content))
	     (type (find-header-class field-name))
	     (hdr (make-instance type :original header :content content)))
	(when (typep hdr 'unknown-header)
	  (setf (header-name hdr) (intern (string-upcase field-name) 'rfc2822)))
	(parse-header hdr)
	hdr))))

(defun parse-headers (string)
  (with-input-from-string (stream string)
    (loop named suck
	  with head = (make-array 0 :element-type 'header :adjustable t :fill-pointer 0)
	  for line = (read-line-unfolding stream)
	  until (string= line "")
	  do (vector-push-extend (parse-header line) head)
	  finally (return-from suck head))))

(defmethod message-headers ((message t))
  t)

(defmethod message-body ((message t))
  t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(base-entity-container entity-original-headers entity-original-body
	    divide-entity-container remove-comments read-line-unfolding parse-header parse-headers *crlf-crlf*
	    message-headers message-body)))


