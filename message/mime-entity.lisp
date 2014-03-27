(in-package mime)

(defclass base-mime-entity (mail-base:base-entity-container)
  ((content-type
    :initarg :content-type
    :initform nil
    :type list
    :accessor entity-content-type)
   (content-transfer-encoding
    :initarg :content-transfer-encoding
    :initform nil
    :type symbol
    :accessor entity-content-transfer-encoding)
   (content-id
    :initarg :content-id
    :initform nil
    :type string
    :accessor entity-content-id)
   (content-description
    :initarg :content-description
    :initform nil
    :type string
    :accessor entity-content-description)
   (content-disposition
    :initarg :content-disposition
    :initform nil
    :type symbol
    :accessor entity-content-disposition)
   (headers
    :accessor entity-headers
    :initarg :headers
    :type vector)))

(defmethod header-value ((entity base-mime-entity) (header-name symbol))
  (header-value (entity-headers entity) header-name))

(defmethod (setf header-value) (new-value (entity base-mime-entity) (header-name symbol))
  (setf (header-value (entity-headers entity) header-name) new-value))

;; for type :multipart body is a list of mime-entitys.  for type :message body is a single mime-entity.  for all other types body is a string

(defclass base-discrete-mime-entity (base-mime-entity)
  ((body
    :initarg :body
    :initform ""
    :type string
    :accessor entity-body)))

(defmethod mime-entity-class-for-content-type ((content-type (eql :default)))
  'text-content-mime-entity)

(defmethod summarize-entity ((entity base-discrete-mime-entity) (as (eql :list)))
  (with-slots (content-type body) entity
    (list (first content-type)
	  (second content-type)
	  body)))

(defstruct (body-part-outline-value) title entity content-type)

(defun body-part-outline-item-title (entity)
  (with-slots (content-type content-description) entity
    (flet ((content-type-string ()
	     (string-downcase (concatenate 'string (symbol-name (first content-type)) "/" (symbol-name (second content-type))))))
      (if (string= content-description "")
	  (case (first content-type)
	    (:text (case (second content-type)
		     (:plain "Plaintext")
		     (:html "HTML")
		     (t (content-type-string))))
	    (:multipart (string-capitalize (symbol-name (second content-type)))))
	  content-description))))

(defun make-content-type-pair (content-type)
  (cons (first content-type)
	(second content-type)))

(defmethod summarize-entity ((entity base-discrete-mime-entity) (as (eql :outline)))
  (with-slots (content-type body) entity
    (make-instance 'cg:outline-item
		   :value (make-body-part-outline-value :title (body-part-outline-item-title entity)
							:entity entity
							:content-type (make-content-type-pair content-type))
		   :state :closed)))

(defclass base-composite-mime-entity (base-mime-entity)
  ())

(defclass unknown-mime-entity (base-mime-entity)
  ((body
    :initarg :body
    :initform ""
    :type string
    :accessor entity-body)))

(defmethod summarize-entity ((entity unknown-mime-entity) (as (eql :list)))
  (with-slots (content-type body) entity
    (list (first content-type)
	  (second content-type)
	  body)))

(defmethod summarize-entity ((entity unknown-mime-entity) (as (eql :outline)))
  (with-slots (content-type body) entity
    (make-instance 'cg:outline-item
		   :value (make-body-part-outline-value :title (body-part-outline-item-title entity)
							:entity entity
							:content-type (make-content-type-pair content-type))
		   :state :closed)))

(defmethod mime-entity-class-for-content-type ((content-type t))
  'unknown-mime-entity)

(defclass text-content-mime-entity (base-discrete-mime-entity)
  ())

(defmethod mime-entity-class-for-content-type ((content-type (eql :text)))
  'text-content-mime-entity)

(defclass image-content-mime-entity (base-discrete-mime-entity)
  ())

(defmethod mime-entity-class-for-content-type ((content-type (eql :image)))
  'image-content-mime-entity)

(defclass audio-content-mime-entity (base-discrete-mime-entity)
  ())

(defmethod mime-entity-class-for-content-type ((content-type (eql :audio)))
  'audio-content-mime-entity)

(defclass video-content-mime-entity (base-discrete-mime-entity)
  ())

(defmethod mime-entity-class-for-content-type ((content-type (eql :video)))
  'video-content-mime-entity)

(defclass application-content-mime-entity (base-discrete-mime-entity)
  ())

(defmethod mime-entity-class-for-content-type ((content-type (eql :application)))
  'application-content-mime-entity)

(defclass message-content-mime-entity (base-composite-mime-entity)
  ((body
    :initarg :body
    :initform ""
    :type base-mime-entity
    :accessor entity-body)))

(defmethod mime-entity-class-for-content-type ((content-type (eql :message)))
  'message-content-mime-entity)

(defmethod parse-mime-body ((original-body string) (headers vector) (entity-type (eql :message)))
  (parse-mime-message original-body))

(defmethod summarize-entity ((entity message-content-mime-entity) (as (eql :list)))
  (with-slots (content-type body) entity
    (list (first content-type)
	  (second content-type)
	  (summarize-entity body as))))

(defmethod summarize-entity ((entity message-content-mime-entity) (as (eql :outline)))
  (with-slots (content-type body) entity
    (make-instance 'cg:outline-item
		   :value (make-body-part-outline-value :title (body-part-outline-item-title entity)
							:entity entity
							:content-type (make-content-type-pair content-type))
		   :state :open
		   :range (list (summarize-entity body as)))))

(defclass multipart-content-mime-entity (base-composite-mime-entity)
  ((body
    :initarg :body
    :initform nil
    :type list
    :accessor entity-body)))

(defmethod mime-entity-class-for-content-type ((content-type (eql :multipart)))
  'multipart-content-mime-entity)

(defmethod parse-mime-body ((original-body string) (headers vector) (entity-type (eql :multipart)))
  (let* ((type-header (find-header 'content-type-header headers))
	 (boundary (cdr (assoc :boundary (header-parameter-list type-header)))))
    (mapcar #'parse-mime-entity (split-body-by-boundary original-body boundary))))

(defmethod summarize-entity ((entity multipart-content-mime-entity) (as (eql :list)))
  (with-slots (content-type body) entity
    (list (first content-type)
	  (second content-type)
	  (mapcar (rcurry #'summarize-entity as) body))))

(defmethod summarize-entity ((entity multipart-content-mime-entity) (as (eql :outline)))
  (with-slots (content-type body) entity
    (make-instance 'cg:outline-item
		   :value (make-body-part-outline-value :title (body-part-outline-item-title entity)
							:entity entity
							:content-type (make-content-type-pair content-type))
		   :state :open
		   :range (mapcar (rcurry #'summarize-entity as) body))))

(defmethod parse-mime-body ((original-body string) (headers vector) (entity-type t))
  original-body)

(defun make-mime-entity (original-headers original-body &key (class-chooser 'mime-entity-class-for-content-type))
  (let* ((parsed-headers (parse-headers original-headers))
	 (entity-type (determine-entity-type-from-headers parsed-headers))
	 (entity (make-instance (funcall (symbol-function class-chooser) entity-type)
				:original-headers original-headers
				:headers parsed-headers
				:original-body original-body)))
      (with-header-values (content-type content-transfer-encoding content-id content-description content-disposition)
	  entity
	(setf (entity-content-type entity) (or content-type '(:text :plain))
	      (entity-content-transfer-encoding entity) (or content-transfer-encoding :7bit)
	      (entity-content-id entity) (or content-id "") ; generate an id locally?
	      (entity-content-description entity) (or content-description "")
	      (entity-content-disposition entity) (or content-disposition :inline)
	      (entity-body entity) (parse-mime-body original-body parsed-headers entity-type)))
      entity))

(defun parse-mime-entity (body-part)
  (multiple-value-bind (headers body-part)
      (mail-base:divide-entity-container body-part)
    (make-mime-entity headers body-part)))

(defun determine-entity-type-from-headers (headers)
  (let ((hdr (find-header 'content-type-header headers)))
    (if hdr
	(header-type hdr)
	:default)))

(defmethod summarize-entity ((entity null) (as t))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(base-mime-entity base-discrete-mime-entity base-composite-mime-entity unknown-mime-entity
	    text-content-mime-entity image-content-mime-entity audio-content-mime-entity video-content-mime-entity application-content-mime-entity
	    message-content-mime-entity multipart-content-mime-entity
	    determine-entity-type-from-headers)))


