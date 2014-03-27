(in-package mail-store)

(define-typetag :archive-message 3100)

(def-view-class archive-message (identifiable-row)
  ((headers
    :accessor archive-message-headers
    :type string
    :initarg :headers)
   (body
    :accessor archive-message-body
    :type string
    :initarg :body)
   (message-ptr
    :accessor archive-message-message-ptr
    :type string
    :initarg :message-ptr)))

(defparameter *mail-backup-directory* #P"z:/backup-mail/")

(defun backup-message (message-text)
  (with-open-file (f (merge-pathnames (format nil "~D-~D" (get-universal-time) (random 65536)) *mail-backup-directory*)
		     :direction :output :if-does-not-exist :create :if-exists :error)
      (write-sequence message-text f)
    nil))

(defun archive-message (message-text)
  (backup-message message-text)
  (with-database (db :application)
    (multiple-value-bind (headers body)
	(rfc2822-parser:divide-entity-container message-text)
      (let ((archive (make-instance 'archive-message :headers headers :body body))
	    (db-base:*lisp-needs-insert-id* t))
	(update-records-from-instance archive :database db)
	archive))))

(defun update-archive-message-message-ptr (id new-ptr)
  (with-database (db :application)
    (execute-r (format nil "UPDATE archive_message SET message_ptr = ~D WHERE id = ~D;" new-ptr id) :db db)))

(defun find-archive-message (archive-id)
  (with-database (db :application)
    (caar (lookup-by-attribute 'archive-message 'id archive-id :db db))))

(defmethod archive-message-for-message ((id integer))
  (with-database (db :application)
    (let ((amid (caar (query-r (format nil "SELECT archive_message_ptr FROM message WHERE id = ~D;" id) :db db))))
      (find-archive-message amid))))

(defun print-message (archive-id &optional (stream t))
  (with-database (db :application)
    (let ((msg (caar (db-base:lookup-by-attribute 'archive-message 'db-base:id archive-id :db db))))
      (format stream "~A~%~A" (archive-message-headers msg) (archive-message-body msg)))))

(defun original-message (archive-id)
  (with-database (db :application)
    (caar (query-r (format nil "SELECT CONCAT_WS('~%', headers, body) FROM archive_message WHERE id = ~D;" archive-id) :db db))))

(defun message-search (search-string
		       &key (in :both) (boolean t) (failover-to-boolean t) (limit nil) (offset nil) 
		       (range nil) (direction :descending))
  (let ((search (sqlquote search-string)))
    (labels ((psearch-1 (column boolean)
	       (search-for-message search :boolean boolean :column column :limit limit :offset offset :range range :direction direction))
	     (psearch (in boolean)
	       (values-list
		(case in
		  (:both (multiple-value-list (psearch-1 "headers, body" boolean)))
		  (:body (multiple-value-list (psearch-1 "body" boolean)))
		  (:headers (multiple-value-list (psearch-1 "headers" boolean)))))))
      (if boolean
	  (psearch in t)
	  (let ((results (multiple-value-list (psearch in nil))))
	    (if (null (first results))
		(if failover-to-boolean
		    (psearch in t)
		    nil)
		(values-list results)))))))

(defun search-for-message (search &key (column nil) (limit nil) (offset nil) (order-by "id") (direction :descending) (boolean nil) (range nil))
  (with-database (db :application)
    (with-tables-locked (db ("archive_message" :read))
      (mapcar #'car
	      (query-r (sqlexpr "SELECT "
					;				(if compute-row-count
					;				    "SQL_CALC_FOUND_ROWS "
					;				    "")
				"message_ptr"
					;				(if boolean
					;				    ", COALESCE(0, 0) AS score"
					;				    (format nil ", MATCH (~A) AGAINST ('~A') AS score" column search))
				" FROM archive_message WHERE "
				(sqlexpr "MATCH (" column ") AGAINST ('" search "'"
					 (if boolean
					     " IN BOOLEAN MODE)"
					     ")"))
				(if range
				    (sqlexpr " AND message_ptr IN "
					     (apply #'sqlgroup range))
				    "")
				(if order-by
				    (format nil " ORDER BY ~A ~A"
					    (if (and boolean (string-equal order-by "score"))
						"id"
						order-by)
					    (case direction
					      (:ascending "ASC")
					      (:descending "DESC")))
				    "")
				(if limit
				    (format nil " LIMIT ~D" limit)
				    "")
				(if offset
				    (format nil " OFFSET ~D" offset)
				    "")
				";")
		       :db db)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(archive-message archive-message-headers archive-message-body find-archive-message print-message original-message
	    archive-message-for-message message-search)))

