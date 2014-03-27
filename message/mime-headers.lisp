(in-package mime)

(defun parse-parameter-list (parameter-list-string)
  (with-input-from-string (s parameter-list-string)
    (let ((current-token (make-array 100 :element-type 'character :adjustable t :fill-pointer 0))
	  (parameters))
      (flet ((push-param ()
	       (push (copy-seq current-token) parameters)
	       (setf (fill-pointer current-token) 0)))
	(loop with inside-parameter = nil
	      with inside-quotes = nil
	      for char = (read-char s nil :eof)
	      until (eq char :eof)
	      do (cond ((and (char= char #\;)
			     (not inside-quotes))
			(if inside-parameter
			    (progn (push-param)
				   (setf inside-parameter nil))
			    (setf inside-parameter t)))
		       ((char= char #\")
			(if inside-quotes
			    (progn (setf inside-quotes nil)
				   (push-param))
			    (setf inside-quotes t)))
		       ((and (member char '(#\Space #\Tab #\Newline) :test #'char=)
			     (not inside-quotes))
			t)
		       (t (vector-push char current-token)))
	      finally (push-param)
		      (return parameters)))
      (loop for parm in parameters
	    for equals = (position #\= parm :test #'char=)
	    when equals
	      collect (cons (intern (string-upcase (subseq parm 0 equals)) 'keyword)
			    (subseq parm (1+ equals)))))))

(defclass mime-header (mail-base:header)
  ()
  (:documentation "Standard header defined in any MIME RFC."))

(defclass rfc2045-header (mime-header)
  ()
  (:documentation "Standard header defined in RFC2045."))

(define-header-class ("MIME-Version" rfc2045-header)
  ((version-number
    :initform nil
    :type string
    :accessor header-version-number)))

(defmethod parse-header ((header mime-version-header))
  (with-slots (version-number content) header
    (setf version-number content)))

(defmethod value-for-header ((hdr mime-version-header))
  (header-version-number hdr))

(define-header-class ("Content-Type" rfc2045-header)
  ((type
    :type symbol
    :initform nil
    :accessor header-type)
   (subtype
    :Type symbol
    :initform nil
    :accessor header-subtype)
   (parameter-list
    :type list
    :initform nil
    :accessor header-parameter-list)))

(defmethod parse-header ((header content-type-header))
  (with-slots (type subtype parameter-list mail-base::content) header
    (setf mail-base::content (mail-base:remove-comments mail-base::content))
    (multiple-value-bind (success whole type/subtype parameters)
	(match-re "([^;]*)(.*)" mail-base::content :return :string)
      (declare (ignore success whole))
      (let ((slash (position #\/ type/subtype :test #'char=))
	    (parms (parse-parameter-list parameters)))
	(setf type (intern  (string-upcase (string-trim '(#\Space #\Tab #\Newline) (subseq type/subtype 0 slash))) 'keyword)
	      subtype (intern (string-upcase (string-trim '(#\Space #\Tab #\Newline) (subseq type/subtype (1+ slash)))) 'keyword)
	      parameter-list parms)))))

(defmethod value-for-header ((hdr content-type-header))
  (with-slots (type subtype parameter-list) hdr
    (list type subtype parameter-list)))

(define-header-class ("Content-Transfer-Encoding" rfc2045-header)
  ((mechanism
    :type symbol
    :accessor header-mechanism)))

(defmethod parse-header ((header content-transfer-encoding-header))
  (with-slots (mechanism content) header
    (setf mechanism (intern (string-upcase content) 'keyword))))

(defmethod value-for-header ((hdr content-transfer-encoding-header))
  (with-slots (mechanism) hdr
    mechanism))

(define-header-class ("Content-ID" rfc2045-header)
  ((id
    :initform nil
    :type string
    :accessor header-id)))

(defmethod parse-header ((header content-id-header))
  (with-slots (id content) header
    (setf id content)))

(defmethod value-for-header ((hdr content-id-header))
  (with-slots (id) hdr
    id))

(define-header-class ("Content-Description" rfc2045-header)
  ((text
    :initform nil
    :type string
    :accessor header-text)))

(defmethod parse-header ((header content-description-header))
  (with-slots (content text) header
    (setf text content)))

(defmethod value-for-header ((hdr content-description-header))
  (with-slots (text) hdr
    text))

(define-header-class ("Content-Disposition" mime-header) ; rfc2183
  ((type
    :initform nil
    :type symbol
    :accessor header-type)
   (parameter-list
    :initform nil
    :type list
    :accessor header-parameter-list)))

(defmethod value-for-header ((hdr content-disposition-header))
  (with-slots (type parameter-list) hdr
    (list type parameter-list)))

(define-header-class ("Content-Location" mime-header) ; rfc2557
  ())
