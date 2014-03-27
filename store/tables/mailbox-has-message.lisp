(in-package mail-store)

(define-typetag :mailbox-has-message 3230)

(def-view-class mailbox-has-message (base-row)
  ((mailbox-ptr
    :accessor mailbox-has-message-mailbox-ptr
    :type integer
    :initarg :mailbox-ptr
    :documentation "ID of mailbox which contains a message.")
   (message-ptr
    :accessor mailbox-has-message-message-ptr
    :type integer
    :initarg :message-ptr
    :documentation "ID of message contained by mailbox.")
   (marked-deleted
    :accessor mailbox-has-message-marked-deleted
    :type boolean
    :initarg :marked-deleted
    :documentation "Whether this message has been marked deleted in this mailbox.")))

(defmethod delete-mailbox-has-message (id)
  (with-database (db :application)
    (execute-r (sqlexpr "DELETE FROM mailbox_has_message WHERE id = "
			id ";")
	       :db db)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mailbox-has-message mailbox-has-message-mailbox-ptr mailbox-has-message-message-ptr delete-mailbox-has-message)))

