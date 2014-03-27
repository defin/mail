(in-package mail-store)

(define-typetag :draft-message 3150)

(def-view-class draft-message (identifiable-row timestamped-row)
  ((headers
    :accessor draft-message-headers
    :type string
    :initarg :headers)
   (body
    :accessor draft-message-body
    :type string
    :initarg :body)))

(defmethod store ((dm draft-message))
  (with-database (db :application)
    (let ((*lisp-needs-insert-id* t))
      (update-records-from-instance dm :database db)
      dm)))

(defun store-draft-message (&rest initargs)
  (store (apply #'make-instance 'draft-message initargs)))

(defun update-draft-message (id &key headers body)
  (with-database (db :application)
    (execute-r (sqlexpr "UPDATE draft_message SET headers = '" (sqlquote headers) "', body = '" (sqlquote body) "' WHERE id = " id ";")
	       :db db)))

(defmethod find-draft-message ((id integer))
  (with-database (db :application)
    (caar (lookup-by-attribute 'draft-message 'id id :database db))))

(defmethod delete-draft-message ((id integer))
  (with-database (db :application)
    (execute-r (sqlexpr "DELETE FROM draft_message WHERE id = " id ";")
	       :db db)))

(defun list-draft-messages ()
  (with-database (db :application)
    (query-r (sqlexpr "SELECT id, headers FROM draft_message ORDER BY id DESC;")
	     :db db)))

(defun summarize-draft-messages ()
  (let ((drafts (list-draft-messages)))
    (loop for (id headers-string) in drafts
	  for headers = (read-from-string headers-string)
	  collect (list :id id :to (getf headers :to) :from (getf headers :from) :subject (getf headers :subject)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(draft-message-headers draft-message-body draft-message
	    list-draft-messages delete-draft-message find-draft-message store-draft-message update-draft-message summarize-draft-messages)))

