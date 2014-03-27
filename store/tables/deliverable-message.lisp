(in-package mail-store)

(define-typetag :deliverable-message 3160)

(def-view-class deliverable-message (base-row)
  ((message-ptr
    :accessor deliverable-message-message-ptr
    :type integer
    :initarg :message-ptr
    :documentation "Pointer to message this was generated from.")
   (host
    :accessor deliverable-message-host
    :type string
    :initarg :host)
   (protocol
    :accessor deliverable-message-protocol
    :type string
    :initarg :protocol)
   (envelope-from
    :accessor deliverable-message-envelope-from
    :type string
    :initarg :envelope-from)
   (envelope-to
    :accessor deliverable-message-envelope-to
    :type string
    :initarg :envelope-to)
   (data
    :accessor deliverable-message-data
    :type string
    :initarg :data)
   (status ; ENUM('queued', 'attempting', 'retry', 'failed', 'delivered') NOT NULL
    :accessor deliverable-message-status
    :type string
    :initarg :status)
   (retry-count                                           ; FIXME eliminate retry logic.. messages should be tried ONCE and then fail for human examination.
    :accessor deliverable-message-retry-count
    :type integer)))

(defmethod store ((message deliverable-message))
  (with-database (db :application)
    (let ((*lisp-needs-insert-id* t))
      (update-records-from-instance message :database db)
      message)))

(defmethod find-deliverable-message ((id integer))
  (with-database (db :application)
    (caar (lookup-by-attribute 'deliverable-message 'id id :database db))))

(defun create-deliverable-message (&rest initargs)
  (store (apply #'make-instance 'deliverable-message initargs)))

(defun list-available-deliverable-messages ()
  (with-database (db :application)
    (mapcar #'car (query-r (sqlexpr "SELECT id FROM deliverable_message WHERE status = 'queued' OR status = 'retry';")
			   :db db))))

(defun list-retryable-messages ()
  (with-database (db :application)
    (mapcar #'car (query-r (sqlexpr "SELECT id FROM deliverable_message WHERE status = 'retry';")
			   :db db))))

(defvar *max-attempt-duration* (* 60 10)) ; 10 minutes

(defun list-stuck-deliverable-messages ()
  ;; a stuck message is either failed or has been in 'attempting' status for over 10 minutes
  (with-database (db :application)
    (query-r (sqlexpr "SELECT id FROM deliverable_message WHERE (status = 'failed') OR (status = 'attempting' AND modification_time < "
		      (- (get-local-time) *max-attempt-duration*)
		      ");")
	     :db db)))

(defun claim-message-for-delivery (id)
  (with-transaction (db :application)
    (with-tables-locked (db ("deliverable_message" :write))
      (let ((status (caar (query-r (sqlexpr "SELECT status FROM deliverable_message WHERE id = " id ";") :db db))))
	(if (or (string= "queued" status) (string= "retry" status))
	    (progn (execute-r (sqlexpr "UPDATE deliverable_message SET status = 'attempting', modification_time = "
				       (get-local-time) " WHERE id = " id ";")
			      :db db)
		   t)
	    nil)))))

(defvar *max-delivery-retries* 10)

(defun set-message-retry-failed (deliverable-message)
  (with-database (db :application)
    (let ((retry-count (incf (deliverable-message-retry-count deliverable-message))))
      (if (>= retry-count *max-delivery-retries*)
	  (progn (execute-r (sqlexpr "UPDATE deliverable_message SET status = 'failed', retry_count = " retry-count
				     ", modification_time = " (get-local-time)
				     " WHERE id = " (id deliverable-message)
				     ";")
			    :db db)
		 :permanent-failure)
	  (progn (execute-r (sqlexpr "UPDATE deliverable_message SET status = 'retry', retry_count = " retry-count
				     ", modification_time = " (get-local-time)
				     " WHERE id = " (id deliverable-message)
				     ";")
			    :db db)
		 :retryable-failure)))))

(defun set-message-delivered (deliverable-message)
  (with-database (db :application)
    (execute-r (sqlexpr "UPDATE deliverable_message SET status = 'delivered', modification_time = "
			(get-local-time)
			" WHERE id = "
			(id deliverable-message)
			";")
	       :db db)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(deliverable-message find-deliverable-message create-deliverable-message
	    deliverable-message-message-ptr deliverable-message-host deliverable-message-protocol deliverable-message-envelope-from
	    deliverable-message-envelope-to deliverable-message-data deliverable-message-status deliverable-message-retry-count
	    set-message-delivered set-message-retry-failed claim-message-for-delivery
	    list-stuck-deliverable-messages list-retryable-messages list-available-deliverable-messages)))
