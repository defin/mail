(in-package mime)

(defclass base-mime-message (base-mime-entity)
  ())

(defclass base-discrete-mime-message (base-mime-message base-discrete-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mail-base:message-headers ((message base-discrete-mime-message))
  (entity-headers message))

(defmethod (setf mail-base:message-headers) ((headers vector) (message base-discrete-mime-message))
  (setf (entity-headers message) headers))

(defmethod mail-base:message-body ((message base-discrete-mime-message))
  (entity-body message))

(defmethod (setf mail-base:message-body) ((body string) (message base-discrete-mime-message))
  (setf (entity-body message) body))

(defclass base-composite-mime-message (base-mime-message base-composite-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mail-base:message-headers ((message base-composite-mime-message))
  (entity-headers message))

(defmethod (setf mail-base:message-headers) ((headers vector) (message base-composite-mime-message))
  (setf (entity-headers message) headers))

(defmethod mail-base:message-body ((message base-composite-mime-message))
  (entity-body message))

(defmethod (setf mail-base:message-body) ((body string) (message base-composite-mime-message))
  (setf (entity-body message) body))

(defclass unknown-mime-message (base-mime-message unknown-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mail-base:message-headers ((message unknown-mime-message))
  (entity-headers message))

(defmethod (setf mail-base:message-headers) ((headers vector) (message unknown-mime-message))
  (setf (entity-headers message) headers))

(defmethod mail-base:message-body ((message unknown-mime-message))
  (entity-body message))

(defmethod (setf mail-base:message-body) ((body string) (message unknown-mime-message))
  (setf (entity-body message) body))

(defmethod mime-message-class-for-content-type ((content-type t))
  'unknown-mime-message)

(defclass text-content-mime-message (base-discrete-mime-message text-content-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mime-message-class-for-content-type ((content-type (eql :text)))
  'text-content-mime-message)

(defmethod mime-message-class-for-content-type ((content-type (eql :default)))
  'text-content-mime-message)

(defclass image-content-mime-message (base-discrete-mime-message image-content-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mime-message-class-for-content-type ((content-type (eql :image)))
  'image-content-mime-message)

(defclass audio-content-mime-message (base-discrete-mime-message audio-content-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mime-message-class-for-content-type ((content-type (eql :audio)))
  'audio-content-mime-message)

(defclass video-content-mime-message (base-discrete-mime-message video-content-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mime-message-class-for-content-type ((content-type (eql :video)))
  'video-content-mime-message)

(defclass application-content-mime-message (base-discrete-mime-message application-content-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mime-message-class-for-content-type ((content-type (eql :application)))
  'application-content-mime-message)

(defclass multipart-content-mime-message (base-composite-mime-message multipart-content-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mime-message-class-for-content-type ((content-type (eql :multipart)))
  'multipart-content-mime-message)

(defclass message-content-mime-message (base-composite-mime-message message-content-mime-entity #+nil rfc2822::rfc2822-message)
  ())

(defmethod mime-message-class-for-content-type ((content-type (eql :message)))
  'message-content-mime-message)

(defun make-mime-message (headers body)
  (make-mime-entity headers body :class-chooser 'mime-message-class-for-content-type))

(defun parse-mime-message (message)
  (multiple-value-bind (headers body)
      (mail-base:divide-entity-container message)
    (make-mime-message headers body)))

; multipart/alternative
;(parse-mime-mail (rutils::file->string "z:/backup-mail/3357001579-17965"))
