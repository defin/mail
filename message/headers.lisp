(in-package mail-base)

(defclass header ()
  ((content :initarg :content
	    :accessor header-content
	    :documentation "Unparsed content of header.")
   (original :initarg :original
	     :accessor header-original
	     :documentation "Complete text of header as read from message.")))

(defmethod print-object ((hdr header) (s stream))
  (print-unreadable-object (hdr s :type t :identity nil)
    (format s "\"~A\"" (if (slot-boundp hdr 'original) 
			   (header-original hdr)
			   (header-content hdr)))))

(defmethod header-name ((header header))
  (type-of header))

(defmethod parse-header ((header header)) 
  t)

(defun find-header (header header-list)
  (or (find header header-list :key #'type-of)
      (find header header-list :key #'header-name :test #'equal)))

(defvar *header-class-table* (make-hash-table :test 'string-equal))

(defun register-header-class (name class)
  (setf (gethash name *header-class-table*) class))

(defun find-header-class (name)
  (let ((header (typecase name
		  (string name)
		  (symbol (symbol-name name)))))
    (gethash header *header-class-table* 'unknown-header)))

(defmacro define-header-class ((name superclass) &body slots)
  (let ((class-name (fintern "~A-HEADER" name)))
    `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,class-name (,superclass)
	 ,@slots)
       (defmethod header-pretty-name ((header ,class-name)) ,name)
       (register-header-class ,name ',class-name)
       (export '(,class-name))
       ',class-name)))

(defclass unknown-header (header)
  ((name :initarg :name
	 :accessor header-name)))

(defmethod value-for-header ((header unknown-header))
  ;; ;; somehow need to find the header of the proper name .. maybe i need unknown-value-for-header which takes a name argument..
  ;;  .. or maybe heade-rvalue itself should take a name argument and a message, more like slot-value
)

(defmethod header-pretty-name ((header unknown-header))
  (capitalize-delimited-string (symbol-name (header-name header)) #\-))


(defmethod value-for-header ((hdr header))
  t)

(defmethod (setf value-for-header) (new-value (hdr header))
  (declare (ignore new-value))
  t)

(defmacro with-header-values ((&rest header-names) entity &body body)
  (let ((bindings (loop for header-name in header-names
			collect `(,header-name (header-value ,entity ',header-name)))))
    `(let ,bindings
       ,@body)))

(defmethod header-value ((headers vector) (header-name symbol))
  (let ((hdr (find-header (find-header-class header-name) headers)))
    (when hdr
      (value-for-header hdr))))

(defmethod (setf header-value) (new-value (headers vector) (header-name symbol))
  (let ((hdr (find-header (find-header-class header-name) headers)))
    (unless hdr
      (setf hdr (make-instance (find-header-class header-name)))
      (push hdr headers))
    (setf (value-for-header hdr) new-value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(header header-content content header-original
	    unknown-header
	    define-header-class find-header-class header-pretty-name find-header 
	    header-value value-for-header with-header-values)))
