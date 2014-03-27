(in-package mail-process)

;;; categorize mail .. figure out based on data in headers and/or body what class of message this is, so we can make a scraper for it if possible

(defvar *category-table* (make-hash-table :test 'eq))

(defun register-category (name class)
  (setf (gethash name *category-table*) class))

(defun find-category (name)
  (gethash name *category-table* 'unknown-category))

(defmacro define-category (name class-name)
  `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
     (mail-base::register-header-class ,name ',class-name)
     ',class-name))

;;; Abstract category classes

(defclass category-instance ()
  ())

;;; Instantiable category classes

(defclass uncategorizable-message (category-instance)
  ())

(define-category :uncategorizable 'uncategorizable-message)

(defclass probable-spam (category-instance)
  ())

(define-category :probable-spam 'probable-spam)

;;; Category Testing

(defmethod categorize-message ((message mail-store:message) (rfc2822-message rfc2822-message))
  (let ((categories (lisa-user::infer-message-categorization rfc2822-message)))
    (let ((cat (find t categories :test #'category-test-sanity)))
      ;; TODO make sure theres only one category, if there are more APPLY-UPDATES shouldnt do anything until human intervention
      (setf (mail-store::message-categorization message) (category-test-name cat))
      (make-category-instance (category-test-name cat) cat))
    (db-base:with-database (db :application)
      (db-base:update-records-from-instance message :database db))
    (setf stored message)
    cat))

(defmethod make-category-instance ((cat symbol))
  nil)

(defmethod apply-updates ((cat symbol) (message mail-store:message))
  (let ((scraped (scrape (make-scraper cat)))) ; first check if a scraper for this category has been defined yet
    (save-info scraped t))
  nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(find-category categorize-message
	    category-instance uncategorizable-message uncategorizable-message
	    test-category make-category-instance apply-updates)))

