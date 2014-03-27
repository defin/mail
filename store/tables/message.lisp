(in-package mail-store)

(define-typetag :message 3110)

(def-view-class message (base-row)
  ((archive-message-ptr
    :accessor message-archive-message-ptr
    :type integer
    :initarg :archive-message-ptr
    :documentation "ID of archived message this message was parsed from.")
   (message-id-ptr
    :accessor message-message-id-ptr
    :type integer
    :initarg :message-id-ptr
    :documentation "Pointer to Message-ID of email.")
   (subject
    :accessor message-subject
    :type string
    :initarg :subject)
   (date
    :accessor message-date
    :type integer
    :initarg :date)
   (from-ptr
    :accessor message-from-ptr
    :type integer
    :initarg :from-ptr)
   (to-ptr
    :accessor message-to-ptr
    :type integer
    :initarg :to-ptr)
   (status
    :accessor message-status
    :type string
    :initarg :status)
   (flags ; unseen, answered, deleted, no_reply_needed, review_later
    :accessor message-flags
    :type string
    :initarg :flags)
   (categorization
    :accessor message-categorization
    :type string)
   (is-reply-to-ptr
     :accessor message-is-reply-to-ptr
     :type integer
     :initarg :is-reply-to-ptr)
   (outgoing-p
    :accessor message-outgoing-p
    :type boolean
    :initarg :outgoing-p)
   (line-count :accessor message-line-count
	       :type integer
	       :initarg :line-count)
;   (owner :accessor message-owner
;	  :type string
;	  :initarg :owner)
;   (encryption :accessor message-encryption
;	       :type string
;	       :initarg :encryption)))
   ))

;; messages need owner and allowed-users and permissions (read, reply, forward, export) on them, also a flag for whether the message is decrypted or encrypted

;; to encryupt a message, use the users password as originally entered by them, if its hash matches the has stored in their user entry..
;; messages that have non-null allowed-users should be stored decrypted (?)

(defmethod store ((message message))
  (with-database (db :application)
    (let ((*lisp-needs-insert-id* t))
      (update-records-from-instance message :database db)
      message)))

(defun store-message (&rest initargs)
  (store (apply #'make-instance 'message initargs)))

(defmethod find-message ((id integer))
  (with-database (db :application)
    (caar (lookup-by-attribute 'message 'id id :database db))))

(defmethod find-message ((header string))
  (with-database (db :application)
    (caar (lookup-by-attribute 'message
			       'id
			       (query-r (format nil "SELECT message_ptr FROM message_id WHERE header='~A'" header)
					:db db
					:flatp t
					:field-names nil)
			       :database db)))) ; use subquery!

(defmethod list-messages-newer-than-id (id)
  (with-database (db :application)
    (mapcar #'car (query-r (sqlexpr "SELECT id FROM message WHERE id > " id " ORDER BY id DESC;")
			   :db db))))

(defun message-summary-fields (id)
  (with-database (db :application)
    (destructuring-bind (date from-ptr to-ptr subject flags is-reply-to-ptr outgoing-p line-count)
	(car (query-r (sqlexpr "SELECT date, from_ptr, to_ptr, subject, flags, is_reply_to_ptr, outgoing_p, line_count FROM message WHERE id = " id ";")
		      :db db))
      (values date from-ptr to-ptr subject (comma-seperated-strings-to-list flags) is-reply-to-ptr (if (zerop outgoing-p) nil t) line-count))))

(defvar *recent-mail-max-age* (* 86400 7)) ; 1 week

(define-warm *recent-mail-mailbox-id* (id (find-mailbox "Recent Mail"))
  "Cache of the id for the Recent Mail mailbox.")

(defun count-lines (text)
  (let ((line-count (count #\Newline text :test #'char=)))
    (if (zerop line-count)
	0
	(if (char= (char text (1- (length text))) #\Newline)
	    line-count
	    (1+ line-count)))))

(defun fixup-all-line-counts ()
  (with-database (db :application)
    (loop for i from 1
	  for message = (find-message i)
	  until (null message)
	  for archive-message = (find-archive-message (message-archive-message-ptr message))
	  when archive-message
	    do (execute-r (sqlexpr "UPDATE message SET line_count = "
				   (+ (count-lines (archive-message-headers archive-message))
					    1 ; to account for blank line between headers and body
					    (count-lines (archive-message-body archive-message)))
				   " WHERE id = " (id message) ";")
		       :db db)
	       (print i))))

(defun create-message (archive-message rfc2822-message &key (outgoing-p nil))
  (with-transaction (db :application :connection-failure-action :defer)
    (let* ((message-id-header (rfc2822-parser:message-message-id rfc2822-message))
	   (references-list (rfc2822-parser:message-references rfc2822-message))
	   (in-reply-to (rfc2822-parser:message-in-reply-to rfc2822-message))
	   (from-email-address (apply #'create-email-address (rfc2822-parser:message-from-address rfc2822-message)))
	   (to-email-address (apply #'create-email-address (rfc2822-parser:message-to-address rfc2822-message)))
	   (date (universal-time->local-time (rfc2822-parser:message-date-ut rfc2822-message)))
	   (line-count (+ (count-lines (archive-message-headers archive-message))
			  1 ; to account for blank line between headers and body
			  (count-lines (archive-message-body archive-message))))
	   (message (store-message :archive-message-ptr (id archive-message)
				   :subject (rfc2822-parser:message-subject rfc2822-message)
				   :date date
				   :from-ptr (id from-email-address)
				   :to-ptr (id to-email-address)
				   :status "INITIAL"
				   :is-reply-to-ptr (if in-reply-to
							(let ((msg (find-message (car in-reply-to))))
							  (if msg (id msg) nil))
							nil)
				   :flags (if outgoing-p
					      "unseen"
					      "")
				   :outgoing-p outgoing-p
				   :line-count line-count))
	   (message-id (ensure-message-id (or message-id-header
					      (if outgoing-p
						  (assign-message-id-header from-email-address)
						  (locally-assign-message-id-header from-email-address)))
					  (id message))))
      (update-archive-message-message-ptr (id archive-message) (id message))
      (update-message-id-ptr message (id message-id))
      (dolist (r (remove-duplicates (append references-list in-reply-to) :test #'string=))
	(when r
	  (let ((msgid (find-message-id r)))
	    (create-message-reference message (or msgid (ensure-message-id r nil))))))
      (when (> date (- (get-local-time) *recent-mail-max-age*))
	(add-message *recent-mail-mailbox-id* (id message)))
      message)))

(defun update-message-id-ptr (message message-id-ptr)
  (with-database (db :application)
    (setf (message-message-id-ptr message) message-id-ptr)
    (execute-r (format nil "UPDATE message SET message_id_ptr = ~D WHERE id = ~D;" message-id-ptr (id message)) :db db)))

(defun message-set-flags (message &key (unseen nil) (answered nil) (review-later nil) (no-reply-needed nil) (deleted nil))
  (let ((flags-list nil))
    (when unseen (push "unseen" flags-list))
    (when answered (push "answered" flags-list))
    (when review-later (push "review later" flags-list))
    (when no-reply-needed (push "no reply needed" flags-list))
    (when deleted (push "deleted" flags-list))
    (message-set-flags-list message flags-list)))

(defun message-set-flags-list (message flags-list)
  (with-database (db :application)
    (let ((new-flags (comma-seperated-list (loop for f in flags-list unless (null f) collect f))))
      (setf (message-flags message) new-flags)
      (execute-r (format nil "UPDATE message SET flags = '~A' WHERE id = ~D;" new-flags (id message)) :db db))))

(defun message-flags-list (message)
  (let ((flags (message-flags message)))
    (comma-seperated-strings-to-list flags)))

(defun message-add-flag (message flag)
  (let ((flags (message-flags-list message)))
    (unless (find flag flags :test #'string=)
      (push flag flags)
      (message-set-flags-list message flags))
    t))

(defun message-delete-flag (message flag)
  (let ((flags (message-flags-list message)))
    (when (find flag flags :test #'string=)
      (message-set-flags-list message (remove flag flags :test #'string=)))
    t))

(defmethod message-body ((message message))
  (archive-message-body (find-archive-message (message-archive-message-ptr message))))

(defmethod message-id-header ((message message))
  (message-id-header (find-message-id (message-message-id-ptr message))))

(defmethod message-from ((message message))
  (email-address-text (find-email-address (message-from-ptr message))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(message message-archive-message-ptr message-message-id-ptr message-subject message-date message-from-ptr message-to-ptr
	    message-status message-flags message-categorization message-is-reply-to-ptr message-outgoing-p
	    message-set-flags-list message-flags-list message-add-flag message-delete-flag
	    find-message store-message message-summary-fields create-message message-body message-id-header message-from
	    comma-seperated-strings-to-list list-messages-newer-than-id)))
