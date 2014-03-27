(in-package mail-store)

(define-typetag :message-reference 3280)

(def-view-class message-reference (identifiable-row)
  ((referencing-message-ptr
    :accessor message-reference-referencing-message-ptr
    :type integer
    :initarg :referencing-message-ptr
    :documentation "Pointer to referencing message.")
   (message-id-ptr
    :accessor message-reference-message-id-ptr
    :type integer
    :initarg :message-id-ptr
    :documentation "Pointer to Message-ID of message referenced.")))

(defmethod create-message-reference ((referencing-message message) (message-id message-id))
  (with-database (db :application)
    (update-records-from-instance (make-instance 'message-reference
						 :referencing-message-ptr (id referencing-message)
						 :message-id-ptr (id message-id))
				  :database db)))

(defmethod list-message-ids-referenced-by-message ((message message))
  (with-database (db :application)
    (query-r (format nil "SELECT message_id_ptr FROM message_reference WHERE referencing_message_ptr = ~D;" (id message))
	     :db db
	     :flatp t
	     :field-names nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(message-reference message-reference-referencing-message-ptr message-reference-message-id-ptr
	    create-message-reference list-message-ids-referenced-by-message)))

