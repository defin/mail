(in-package lisa-user)

(defparameter *mail-categorization-inference-engine* (make-inference-engine))
(defparameter *categorization* nil)

(defun print-address-from-list (list)
  (destructuring-bind (user host phrase) list
    (declare (ignore phrase))
    (concatenate 'string user "@" host)))

(defun infer-message-categorization (rfc2822-message)
  (flet ((content (header headers)
	   (let ((hdr (mail-base:find-header header headers)))
	     (if hdr
		 (rfc2822:header-content hdr)
		 nil))))
    (let* ((headers (rfc2822:rfc2822-message-headers rfc2822-message))
	   (?from-header (print-address-from-list (rfc2822:message-from-address rfc2822-message)))
	   (?to-header (print-address-from-list (rfc2822:message-to-address rfc2822-message)))
	   (?reply-to-header (content 'rfc2822:reply-to-header headers))
	   (?cc-header (content 'rfc2822:cc-header headers))
	   (?subject-header (content 'rfc2822:subject-header headers))
	   (?body-content (rfc2822:message-body rfc2822-message))
	   (*categorization* nil))
      (assert (message-match))
      (when ?from-header (assert (email-address (address-type 'from) (text ?from-header))))
      (when ?to-header (assert (email-address (address-type 'to) (text ?to-header))))
      (when ?reply-to-header (assert (email-address (address-type 'reply-to) (text ?reply-to-header))))
      (when ?cc-header (assert (email-address (address-type 'cc) (text ?cc-header))))
      (when ?subject-header (assert (subject-header (text ?subject-header))))
      (when ?body-content (assert (body-content (text ?body-content))))
      (run)
      (values *categorization*))))
 ; (cdr (assoc '?x (car (retrieve (?x) (?x (message-match))))))))))

;;; for customer service messages, figure out by message-id references, if possible, what thread this message belongs in.. otherwise at least figure out by reply-to line what order/purchase it goes to..

;(with-inference-engine (*mail-categorization-inference-engine*)
 
;(watch :all)

(defmacro define-from-match-rule (symbol string)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defrule ,(intern (format nil "MATCHES-FROM-~A" string) *package*) ()
       (email-address (address-type ?address-type (eql ?address-type 'from))
		      (text ?text (string-equal ?text ,string)))
       (?message-match (message-match (from-matched ?x (null ?x))))
       =>
       (modify ?message-match (from-matched ',symbol)))
    ',symbol))

(defmacro define-subject-match-rule (symbol string)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defrule ,(intern (format nil "SUBJECT-MATCHES-~A" (symbol-name symbol))) ()
       (subject-header (text ?text (search ,string ?text :start2 0 :test #'string-equal)))
       (?message-match (message-match (subject-matched ?x (null ?x))))
       =>
       (modify ?message-match (subject-matched ',symbol)))
     ',symbol))

(defmacro define-body-match-rule (symbol string)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defrule ,(intern (format nil "BODY-MATCHES-~A" (symbol-name symbol))) ()
       (body-content (text ?text (search ,string ?text :test #'string-equal)))
       (?message-match (message-match (body-matched ?body-matched (not-member ',symbol ?body-matched))))
       =>
       (modify ?message-match (body-matched (push ',symbol ?body-matched))))
     ',symbol))

(defmacro define-message-categorization-rule (symbol &key cc from subject body)
  (let ((patterns))
    (when cc (push `(cc-matched ?cc (eql ?cc ',cc)) patterns))
    (when from (push `(from-matched ?from-matched (eql ?from-matched ',from)) patterns))
    (when subject (push `(subject-matched ?subject (eql ?subject ',subject)) patterns))
    (when body (push `(body-matched ?body-matched (member ',body ?body-matched)) patterns))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defrule ,symbol ()
	 (?message-match (message-match ,@patterns
					(categorization ?categorization (not-member ',symbol ?categorization))))
	 =>
	 (modify ?message-match (categorization (push ',symbol ?categorization))))
       ',symbol)))

(defmacro define-simple-categorization-rule (rule-symbol &key from cc subject body)
  (let ((bindings)
	(subrules)
	(from-symbol (if from (intern from *package*) nil))
	(cc-symbol (if cc (intern cc *package*) nil))
	(subject-symbol (if subject (intern subject *package*) nil))
	(body-symbol (if subject (intern body *package*) nil)))
    (when from-symbol
      (push from-symbol bindings)
      (push :from bindings)
      (push `(define-from-match-rule ,from-symbol ,from) subrules))
    (when cc-symbol
      (push cc-symbol bindings)
      (push :cc bindings)
      (push `(define-from-match-rule ,cc-symbol ,cc) subrules))
    (when subject-symbol
      (push subject-symbol bindings)
      (push :subject bindings)
      (push `(define-subject-match-rule ,subject-symbol ,subject) subrules))
    (when body-symbol
      (push body-symbol bindings)
      (push :body bindings)
      (push `(define-body-match-rule ,body-symbol ,body) subrules))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-message-categorization-rule ,rule-symbol ,@bindings)
       ,@subrules
       ',rule-symbol)))

(deftemplate message-match ()
  (slot from-matched)
  (slot to-matched)
  (slot reply-to-matched)
  (slot cc-matched)
  (slot subject-matched)
  (slot subject-has-re)
  (slot body-matched)
  (slot categorization)
  (slot automated))

(deftemplate email-address ()
  (slot address-type)
  (slot text))

(deftemplate subject-header ()
  (slot text))

(deftemplate body-content ()
  (slot text))

(defun local-address-p (email-address)
  (not (not (member email-address '("mail.twenty3.dyndns.org" "twenty3.dyndns.org" "trancell.com" "metronet.com")
		    :test #'(lambda (x y) (search y x :test #'string=))))))

(defun not-member (item list)
  (not (member item list)))

(defrule matches-from-local-address ()
  (email-address (address-type ?address-type (eql ?address-type 'from))
		 (text ?text (local-address-p ?text)))
  (?message-match (message-match (from-matched ?x (null ?x))))
  =>
  (modify ?message-match (from-matched 'local-address)))

(defrule subject-starts-with-re ()
  (subject-header (text ?text (search "Re:" ?text :start2 0 :test #'string=)))
  (?message-match (message-match (subject-has-re ?x (null ?x))))
  =>
  (modify ?message-match (subject-has-re t)))

(defrule uncategorizable (:salience -10)
  (?message-match (message-match (categorization ?categorization (null ?categorization))))
  =>
  (modify ?message-match (categorization (push 'uncategorizable ?categorization))))

(defrule message-match-finished (:salience -20)
  (?message-match (message-match (categorization ?categorization)))
  =>
  (setf *categorization* ?categorization)
  (defrule cleanup-message-categorization-facts (:salience -100)
    (?fact (or (message-match) (email-address) (subject-header) (body-content)))
    =>
    (retract ?fact))
  (defrule cleanup-message-categorization-cleanup-rules (:salience -200)
    (not (email-address))
    (not (message-match))
    (not (subject-header))
    (not (body-content))
    =>
    (undefrule 'cleanup-message-categorization-facts)
    (undefrule 'cleanup-message-categorization-cleanup-rules)))

(reset)
