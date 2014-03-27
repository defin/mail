(in-package mail-store)

(define-typetag :email-address 3290)

(def-view-class email-address (base-row*)
  ((phrase
    :accessor email-address-phrase
    :type string
    :initarg :phrase
    :documentation "Descriptive text appearing with email address.")
   (local-part
    :accessor email-address-local-part
    :type string
    :initarg :local-part
    :documentation "Local part of address.")
   (domain
    :accessor email-address-domain
    :type string
    :initarg :domain
    :documentation "Domain of address.")
   (text
    :accessor email-address-text
    :type string
    :initarg :text
    :documentation "Actual email address.")))

(defmethod find-email-address ((address integer))
  (with-database (db :application)
    (caar (db-base:lookup-by-attribute 'email-address 'id address :database db))))

(defmethod find-email-address ((address string))
  (with-database (db :application)
    (caar (db-base:lookup-by-attribute 'email-address 'text address :database db))))

(defun create-email-address (local-part domain phrase)
  (let* ((text (concatenate 'string local-part "@" domain))
	 (found (find-email-address text))
	 (addr (if found
		   nil
		   (make-instance 'email-address :local-part local-part :domain domain :phrase phrase :text text)))
	 (db-base:*lisp-needs-insert-id* t))
    (if found
	found
	(with-database (db :application)
	  (update-records-from-instance addr :database db)
	  addr))))

(defun ensure-email-address (text)
  (let* ((found (find-email-address text))
	 (addr (if found
		   nil
		   (make-instance 'email-address :text text)))
	 (db-base:*lisp-needs-insert-id* t))
    (if found
	found
	(with-database (db :application)
	  (update-records-from-instance addr :database db)
	  addr))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(email-address email-address-phrase email-address-local-part email-address-domain email-address-text
	    find-email-address create-email-address ensure-email-address)))

