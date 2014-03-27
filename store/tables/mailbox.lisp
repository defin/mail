(in-package mail-store)

(define-typetag :mailbox 3220)

(def-view-class mailbox (base-row)
  ((name
    :accessor mailbox-name
    :type string
    :initarg :name
    :documentation "Name of mailbox.")
   (description
    :accessor mailbox-description
    :type string
    :initarg :description
    :documentation "English description of mailbox.")
   (automatically-generated
    :accessor mailbox-automatically-generated
    :type boolean
    :initarg :automatically-generated
    :initform nil
    :documentation "Whether or not this mailbox was generated automatically.")))

(defclass computed-mailbox (mailbox)
  ((message-list
    :accessor mailbox-message-list
    :type list
    :initform nil
    :initarg :message-list)
   (deleted-message-list
    :accessor mailbox-deleted-message-list
    :type list
    :initform nil
    :initarg :deleted-message-list)))

(defvar *computed-mailbox-list* nil)
(defvar *computed-mailbox-id-counter* 0)

(defclass temporary-mailbox (computed-mailbox)
  ())

(defvar *temporary-mailbox-list* nil) 
(defvar *temporary-mailbox-id-counter* 0)

(defun create-mailbox (name &key (description "") (automatically-generated nil) (temporary nil) (computed nil))
  (with-database (db :application)
    (let ((mbox (make-instance (if temporary
				   'temporary-mailbox
				   'mailbox)
			       :name name
			       :description description
			       :automatically-generated automatically-generated))
	  (*lisp-needs-insert-id* t))
      (if temporary
	  (progn (setf (id mbox) (decf *temporary-mailbox-id-counter*))
		 (push mbox *temporary-mailbox-list*))
	  (update-records-from-instance mbox :database db))
      mbox)))

(defmethod remove-mailbox ((mailbox integer))
  (with-database (db :application)
    (execute-r (sqlexpr "DELETE FROM mailbox WHERE id = "
			mailbox
			";")
	       :db db)))

(defun find-mailbox (name)
  (with-database (db :application)
    (or (etypecase name
	  (string (find name *temporary-mailbox-list* :test #'string-equal :key #'mailbox-name))
	  (integer (find name *temporary-mailbox-list* :test #'= :key #'id)))
	(caar (lookup-by-attribute 'mailbox (etypecase name
					      (string 'name)
					      (integer 'id))
				   name
				   :database db)))))

(defun ensure-mailbox (name &key description automatically-generated temporary computed)
  (aif (find-mailbox name)
       it
       (create-mailbox name :description description :automatically-generated automatically-generated :temporary temporary :computed computed)))
(defmethod mailbox-name ((mailbox integer))
  (with-database (db :application)
    (if (minusp mailbox)
	(find mailbox *temporary-mailbox-list* :test #'= :key #'id)
	(caar (column-value 'mailbox 'name mailbox :db db)))))

(defmethod add-message ((mailbox integer) (message integer))
  (add-message (find-mailbox mailbox) (find-message message)))

(defmethod add-message ((mailbox null) (message integer))
  nil)

(defmethod add-message ((mailbox null) (message message))
  nil)

(defmethod add-message ((mailbox mailbox) (message list))
  (mapc #'(lambda (m) (add-message mailbox m)) message))

(defmethod add-message ((mailbox list) (message message))
  (mapc #'(lambda (m) (add-message m message)) mailbox))

(defmethod add-message ((mailbox list) (message-ptr integer))
  (let ((message (find-message message-ptr)))
    (mapc #'(lambda (m) (add-message m message)) mailbox)))

(defmethod add-message ((mailbox string) (message message))
  (add-message (find-mailbox mailbox) message))

(defmethod add-message ((mailbox temporary-mailbox) (message message))
  (pushnew (id message) (mailbox-message-list mailbox)))

(defmethod add-message ((mailbox mailbox) (message message))
  (with-database (db :application)
    (unless (mailbox-has-message-p (id mailbox) (id message))
      (let ((db-base:*lisp-needs-insert-id* t))
	(update-records-from-instance (make-instance 'mailbox-has-message
						     :mailbox-ptr (id mailbox)
						     :message-ptr (id message))
				      :database db)))))

(defmethod remove-message ((mailbox integer) (message integer))
  (remove-message (find-mailbox mailbox) (find-message message)))

(defmethod remove-message ((mailbox mailbox) (message list))
  (mapc #'(lambda (m) (remove-message mailbox m)) message))

(defmethod remove-message ((mailbox list) (message message))
  (mapc #'(lambda (m) (remove-message m message)) mailbox))

(defmethod remove-message ((mailbox string) (message message))
  (remove-message (find-mailbox mailbox) message))

(defmethod remove-message ((mailbox temporary-mailbox) (message message))
  (pull (id message) (mailbox-message-list mailbox))
  (pull (id message) (mailbox-deleted-message-list mailbox)))

(defmethod remove-message ((mailbox mailbox) (message message))
  (with-database (db :application)
    (execute-r (sqlexpr "DELETE FROM mailbox_has_message WHERE mailbox_ptr = "
			(id mailbox)
			" AND message_ptr = "
			(id message)
			";")
	       :db db)))

(defmethod message-mark-deleted ((mailbox integer) (message integer))
  (message-mark-deleted (find-mailbox mailbox) (find-message message)))

(defmethod message-mark-deleted ((mailbox string) (message message))
  (message-mark-deleted (find-mailbox mailbox) message))

(defmethod message-mark-deleted ((mailbox temporary-mailbox) (message message))
  (pushnew (id message) (mailbox-deleted-message-list mailbox)))

(defmethod message-mark-deleted ((mailbox mailbox) (message message))
  (with-database (db :application)
    (execute-r (sqlexpr "UPDATE mailbox_has_message SET marked_deleted = 1 WHERE mailbox_ptr = "
			(id mailbox)
			" AND message_ptr = "
			(id message)
			";")
	       :db db)))

(defmethod message-unmark-deleted ((mailbox integer) (message integer))
  (message-unmark-deleted (find-mailbox mailbox) (find-message message)))

(defmethod message-unmark-deleted ((mailbox string) (message message))
  (message-unmark-deleted (find-mailbox mailbox) message))

(defmethod message-unmark-deleted ((mailbox temporary-mailbox) (message message))
  (pull (id message) (mailbox-deleted-message-list mailbox)))

(defmethod message-unmark-deleted ((mailbox mailbox) (message message))
  (with-database (db :application)
    (execute-r (sqlexpr "UPDATE mailbox_has_message SET marked_deleted = 0 WHERE mailbox_ptr = "
			(id mailbox)
			" AND message_ptr = "
			(id message)
			";")
	       :db db)))

(defmethod message-marked-deleted-p ((mailbox integer) (message integer))
  (message-marked-deleted-p (find-mailbox mailbox) (find-message message)))

(defmethod message-marked-deleted-p ((mailbox string) (message message))
  (message-marked-deleted-p (find-mailbox mailbox) message))

(defmethod message-marked-deleted-p ((mailbox temporary-mailbox) (message message))
  (find (id message) (mailbox-deleted-message-list mailbox) :test #'=))

(defmethod message-marked-deleted-p ((mailbox mailbox) (message message))
  (with-database (db :application)
    (let ((p (caar (query-r (sqlexpr "SELECT marked_deleted FROM mailbox_has_message WHERE mailbox_ptr = "
				     (id mailbox)
				     " AND message_ptr = "
				     (id message)
				     ";")
			    :db db))))
      (if p
	  (if (zerop p)
	      nil
	      t)
	  nil))))

(defmethod list-message-ptrs ((id integer))
  (list-message-ptrs (find-mailbox id)))

(defmethod list-message-ptrs ((mailbox temporary-mailbox))
  (mailbox-message-list mailbox))

(defmethod list-message-ptrs ((mailbox mailbox))
  (with-database (db :application)
    (query-r (format nil "SELECT message_ptr FROM mailbox_has_message WHERE mailbox_ptr=~D" (id mailbox))
	     :db db
	     :flatp t
	     :names nil)))

(defmethod list-mailboxes-containing ((id integer))
  (with-database (db :application)
    (query-r (format nil "SELECT mailbox_ptr FROM mailbox_has_message WHERE message_ptr=~D" id)
	     :db db
	     :flatp t
	     :names nil)))

(defmethod list-mailboxes-containing ((message message))
  (list-mailboxes-containing (id message)))

(defmethod mailbox-has-message-p ((mailbox integer) (message integer))
  (with-database (db :application)
    (not (not (query-r (sqlexpr "SELECT id FROM mailbox_has_message WHERE mailbox_ptr = "
				mailbox
				" AND message_ptr = "
				message
				";")
		       :db db)))))

(defmethod copy-mailboxes ((from-message message) (to-message message))
  (copy-mailboxes (id from-message) (id to-message)))

(defmethod copy-mailboxes ((from-message integer) (to-message integer))
  (let ((from-mailboxes (list-mailboxes-containing from-message)))
    (mapcar #'(lambda (mbox)
		(add-message mbox to-message))
	    from-mailboxes)))

(defmethod add-message-to-object-mailboxes ((message-ptr integer) &rest objects)
  (let ((mailboxes (loop for object in objects
			 for pretty-name = (object-pretty-name object)
			 collect (ensure-mailbox pretty-name))))
    (add-message mailboxes message-ptr)))

(defun flush-old-messages-from-recent-mail ()
  (with-transaction (db :application)
    (let ((ids (mapcar #'car (query-r (sqlexpr "SELECT id FROM mailbox_has_message WHERE mailbox_ptr = " *recent-mail-mailbox-id*
					       " AND creation_time < " (- (get-local-time) *recent-mail-max-age*) ";")
				      :db db))))
      (mapc #'delete-mailbox-has-message ids))))

(app:define-system-timer :flush-recent-mail
    (:name "Flush old messages from recent mail"
     :period #.(* 60 60 24) ; 1 day
     :target-facility :mail-backend
     :function flush-old-messages-from-recent-mail))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mailbox temporary-mailbox mailbox-name mailbox-description mailbox-reference mailbox-reference-typetag mailbox-automatically-generated
	    create-mailbox find-mailbox ensure-mailbox
	    message-mark-deleted message-unmark-deleted message-marked-deleted-p flush-old-messages-from-recent-mail
	    add-message add-message-to-object-mailboxes remove-message list-message-ptrs list-mailboxes-containing copy-mailboxes)))
