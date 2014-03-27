(in-package rfc2822)

(defclass rfc2822-message (mail-base:base-entity-container)
  ((headers :accessor rfc2822-message-headers
	    :initarg :headers
	    :type vector)
   (body :accessor rfc2822-message-body
	 :initarg :body
	 :type string)))

(defmethod mail-base:message-headers ((message rfc2822-message))
  (rfc2822-message-headers message))

(defmethod (setf mail-base:message-headers) ((headers vector) (message rfc2822-message))
  (setf (rfc2822-message-headers message) headers))

(defmethod mail-base:message-body ((message rfc2822-message))
  (rfc2822-message-body message))

(defmethod (setf mail-base:message-body) ((body string) (message rfc2822-message))
  (setf (rfc2822-message-body message) body))

(defun make-rfc2822-message (headers body)
  (make-instance 'rfc2822-message :original-headers headers :headers (parse-headers headers) :body body))

(defun parse-mail (message)
  (multiple-value-bind (headers body)
      (mail-base:divide-entity-container message)
    (make-rfc2822-message headers body)))

(defmethod header-value ((message rfc2822-message) (header-name symbol))
  (header-value (message-headers message) header-name))

(defmethod (setf header-value) (new-value (message rfc2822-message) (header-name symbol))
  (setf (header-value (message-headers message) header-name) new-value))

; anyone use this?
(defmethod message-header-content ((message rfc2822-message) (header symbol))
  (let ((hdr (find-header header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(rfc2822-message rfc2822-message-headers rfc2822-message-body 
	    make-rfc2822-message parse-mail)))

