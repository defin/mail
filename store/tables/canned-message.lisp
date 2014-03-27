(in-package mail-store)

(define-typetag :canned-message 3120)

(def-view-class canned-message (identifiable-row timestamped-row)
  ((name
    :accessor canned-message-name
    :type string
    :initarg :name)
   (body
    :accessor canned-message-body
    :type string
    :initarg :body)))

(defmethod store ((dm canned-message))
  (with-database (db :application)
    (let ((*lisp-needs-insert-id* t))
      (update-records-from-instance dm :database db)
      dm)))

(defun store-canned-message (&rest initargs)
  (store (apply #'make-instance 'canned-message initargs)))

(defun update-canned-message (id &key name body)
  (with-database (db :application)
    (execute-r (sqlexpr "UPDATE canned_message SET name = '" (sqlquote name) "', body = '" (sqlquote body) "' WHERE id = " id ";")
	       :db db)))

(defmethod find-canned-message ((id integer))
  (with-database (db :application)
    (caar (lookup-by-attribute 'canned-message 'id id :database db))))

(defmethod delete-canned-message ((id integer))
  (with-database (db :application)
    (execute-r (sqlexpr "DELETE FROM canned_message WHERE id = " id ";")
	       :db db)))

(defun list-canned-messages ()
  (with-database (db :application)
    (query-r (sqlexpr "SELECT id, name FROM canned_message ORDER BY name ASC;")
	     :db db)))

(defun summarize-canned-messages ()
  (let ((canneds (list-canned-messages)))
    (loop for (id name) in canneds
	  collect (list :id id :name name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(canned-message-name canned-message-body canned-message
	    list-canned-messages delete-canned-message find-canned-message store-canned-message update-canned-message summarize-canned-messages)))

