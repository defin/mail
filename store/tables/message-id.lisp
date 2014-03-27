(in-package mail-store)

(define-typetag :message-id 3270)

(def-view-class message-id (identifiable-row)
  ((header
    :accessor message-id-header
    :type string
    :initarg :header
    :documentation "Message-ID from header.")
   (message-ptr
    :accessor message-id-message-ptr
    :type integer
    :initarg :message-ptr
    :documentation "Pointer to Message assigned to this Message-ID.")))

(defmethod find-message-id ((header string))
  (with-database (db :application)
    (caar (lookup-by-attribute 'message-id 'header header :database db))))

(defmethod find-message-id ((id integer))
  (with-database (db :application)
    (caar (lookup-by-attribute 'message-id 'id id :database db))))

(defmethod ensure-message-id ((header string) (message-ptr t))
  (with-database (db :application)
    (let ((msgid (find-message-id header)))
      (cond ((null msgid)
	     (setf msgid (make-instance 'message-id
					:header header
					:message-ptr (if message-ptr
							 message-ptr
							 nil)))
	     (let ((*lisp-needs-insert-id* t))
	       (update-records-from-instance msgid :database db)))
	    ((and message-ptr (null (message-id-message-ptr msgid)))
	     (setf (message-id-message-ptr msgid) message-ptr)
	     (execute-r (format nil "UPDATE message_id SET message_ptr = ~D WHERE id = ~D;" message-ptr (id msgid)) :db db))
	    ((and message-ptr (= (message-id-message-ptr msgid) message-ptr))
	     msgid)
	    (t 0)) ;(error "Can't create Message-ID object for ~A, it is already assigned to message ~D." header (message-id-message-ptr msgid))))
      msgid)))

(defun assign-message-id-header (from-address)
  (concatenate 'string
	       "<"
	       (base36-string (get-universal-time))
	       "."
	       (princ-to-string (random 65536))
	       "."
	       from-address
	       ">"))

(defun locally-assign-message-id-header (from-address)
  (concatenate 'string
	       "<"
	       "L."
	       (base36-string (get-universal-time))
	       "."
	       (base36-string (random 65536))
	       "."
	       from-address
	       ">"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(message-id message-id-header message-id-message-ptr find-message-id
	    create-message-id assign-message-id-header locally-assign-message-id-header)))
