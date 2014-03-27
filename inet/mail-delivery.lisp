(in-package mail-inet)

(defparameter *mail-relay-host* "10.5.5.32")

;(defmemoize-expiring mx-rr-lookup (host)

(defun mx-rr-lookup (host)
  (multiple-value-bind (result valid-for other-results)
      (socket:dns-query host :type :mx)
    (values (if (and (numberp valid-for) (minusp valid-for))
		0
		valid-for)
	    result
	    other-results)))

; make sure this is fully compliant with RFC974 .. in the meantime we can punt messages to sendmail to deal with in complex cases.
(defun mx-lookup (host)
  (multiple-value-bind (valid-for result other-results) (mx-rr-lookup host)
    (declare (ignore valid-for other-results))
    (if result ; according to the MX rfc, this needs to be more complex to handle all cases.
	(destructuring-bind (mailhost presolved preference) result
	  (declare (ignore preference))
	  (aif presolved
	       it
	       (socket:lookup-hostname mailhost)))
	(socket:lookup-hostname host))))

(logger:define-log-message :delivering-message (:type :mail :severity :info)
  (message via protocol)
  "Preparing to deliver Message ~D via ~A (~A)."
  message via protocol)

(logger:define-log-message :message-delivered (:type :mail :severity :info)
  (message)
  "Message ~D delivered."
  message)

(logger:define-log-message :message-delivery-failed-permanently (:type :mail :severity :fatal)
  (message)
  "Delivery of Message ~D failed, abandoning delivery."
  message)

(logger:define-log-message :message-delivery-failed-retryable (:type :mail :severity :warning)
  (message)
  "Delivery of Message ~D failed, will try again."
  message)

(logger:define-log-message :message-already-claimed (:type :mail :severity :warning)
  (message)
  "Can't deliver Message ~D, it has already been claimed for delivery."
  message)

(defparameter *test-mode* nil)

(defmethod deliver-message ((msg mail-store:deliverable-message))
  (with-accessors ((id db-base:id)
		   (envelope-from mail-store:deliverable-message-envelope-from)
		   (envelope-to mail-store:deliverable-message-envelope-to)
		   (data mail-store:deliverable-message-data)
		   (host mail-store:deliverable-message-host)
		   (protocol mail-store:deliverable-message-protocol)
		   (message-ptr mail-store:deliverable-message-message-ptr)) msg
    (if (mail-store:claim-message-for-delivery id)
	(progn (logger:!log :delivering-message message-ptr host protocol)
	       (if (cond ((string= protocol "SMTP")
			  (deliver-message-smtp msg))
			 ((string= protocol "HTTP")
			  (deliver-message-http msg)))
		   (progn (mail-store:set-message-delivered msg)
			  (logger:!log :message-delivered message-ptr))
		   (case (mail-store:set-message-retry-failed msg)
		     (:permanent-failure (logger:!log :message-delivery-failed-permanently message-ptr))
		     (:retryable-failure (logger:!log :message-delivery-failed-retryable message-ptr)))))
	(logger:!log :message-already-claimed message-ptr))))

(defmethod deliver-message-smtp ((msg mail-store:deliverable-message))
  (with-accessors ((id db-base:id) (envelope-from mail-store:deliverable-message-envelope-from)
		   (envelope-to mail-store:deliverable-message-envelope-to) (data mail-store:deliverable-message-data)
		   (host mail-store:deliverable-message-host) (protocol mail-store:deliverable-message-protocol)
		   (message-ptr mail-store:deliverable-message-message-ptr)) msg
    (let* ((relay *mail-relay-host*)
;	   (mx (mx-lookup host))
	   (smtp-server relay))
      (if *test-mode*
	  (format t "~&-- MX: ~A, Envelope: From ~A To ~A --~%~A" smtp-server envelope-from envelope-to data)
	  (handler-case
	      (progn (net.post-office:send-smtp smtp-server envelope-from envelope-to data)
		     t)
	    (excl:socket-error (c)
	      (logger:!log :unhandled-error 'socket-error mp:*current-process* c nil)
	      nil))))))

(defmethod deliver-message-http ((msg mail-store:deliverable-message))
  (with-accessors ((id db-base:id) (envelope-from mail-store:deliverable-message-envelope-from)
		   (envelope-to mail-store:deliverable-message-envelope-to) (data mail-store:deliverable-message-data)
		   (host mail-store:deliverable-message-host) (protocol mail-store:deliverable-message-protocol)
		   (message-ptr mail-store:deliverable-message-message-ptr)) msg
    (start-agent :deliver-message-via-webpage host envelope-from envelope-to data)))

#|
(defun retry-delayed-mail ()
  (let ((msgs (mail-store:list-retryable-messages)))
    (dolist (m msgs)
      (deliver-message (mail-store:find-message m)))))

(app:define-system-timer :retry-delayed-mail
    (:name "Retry sending delayed mail"
     :period #.(* 60 60) ; 1 hour
     :target-facility :mail-backend
     :function retry-delayed-mail))

(logger:define-log-message :problem-delivering-messages (:type :mail :severity :fatal)
  (message-list)
  "There have been problems with the following deliverable-messages, please investigate: ~S"
  message-list)

(defun report-stuck-mail ()
  (let ((msgs (mail-store:list-stuck-deliverable-messages)))
    (logger:!log :problem-delivering-messages msgs)))

(app:define-system-timer :report-stuck-mail
    (:name "Report stuck deliverable messages"
     :period #.(* 60 60) ; 1 hour
     :function report-stuck-mail))
|#

#|
(define-cold *mail-delivery-hold* nil "Messages being held pending deliverability.")
(define-cold *abandoned-mail* nil "Messages which are undeliverable, at least without human assistance.")
(define-warm *mail-delivery-active-queue* (make-instance 'mp:queue) "Active queue, updated once a minute by the queue in the database.")
(define-warm *mail-delivery-threads* (make-hash-table :test 'string-equal) "Active threads that are trying to deliver mail.")
;(define-warm *host->smtp-stream-table* (make-hash-table :test 'string-equal) "Table of socket-streams attached to SMTP servers.")

(defparameter *our-mailhost-name* "trancell.com")

(defstruct (mail-msg-out) from to subject message-id message-number internal-id body tty insertion-time hold-time hold-reason hold-duration)

(defun queue-mail-msg-out (&key from to subject body tty insertion-time hold-reason message-number (internal-id nil))
  (let ((msg (make-mail-msg-out :from from :to to :subject subject :body body :tty tty :insertion-time insertion-time
				:hold-reason hold-reason :message-number message-number 
				:internal-id (or internal-id (internal-id message-number)))))
;    (save-outgoing-mail msg)
    (enqueue *mail-delivery-queue* msg)
))

;(defun save-outgoing-mail (msg)
;    (with-open-file (out (merge-pathnames (format nil "~A-~A" *short-host-name* (internal-id *message-counter*))
;					  "tits:playpen;mail-out;") 
;		     :if-exists :error :if-does-not-exist :create)
;      ()))
    
;;; (.)(.) Mail Delivery 
(defun delivery-thread-already-registered-p (mailto)
  (gethash mailto *mail-delivery-threads*))

(defun register-mail-delivery-thread (mailto thread)
  (setf (gethash mailto *mail-delivery-threads*) thread))

(defun unregister-mail-delivery-thread (mailto)
  (remhash mailto *mail-delivery-threads*))

(defun hold-mail-message (msg hold-reason duration)
  (setf (mail-msg-out-hold-reason msg) hold-reason)
  (setf (mail-msg-out-hold-duration msg) duration)
  (setf (mail-msg-out-hold-time msg) (get-universal-time))
  (push msg *mail-delivery-hold*))

(defun held-message-count ()
  (length *mail-delivery-hold*))

(defun abandon-delivery (msg reason)
  (push (cons msg reason) *abandoned-mail*))

(defun mail-message-deliverable-p (msg)
  (let ((hold-reason (mail-msg-out-hold-reason msg)))
    (cond ((or (null hold-reason) (eql hold-reason :host-busy))
	   (not (delivery-thread-already-registered-p (mail-msg-out-to msg)))))))

(defun maybe-deliver-some-held-messages ()
  "Returns T if at least one held message was deliverable, otherwise NIL."
  (when *mail-delivery-hold*
    (if (member t (mapcar #'deliver-message-if-deliverable
			  *mail-delivery-hold*)
		:test #'eq)
	t
        nil)))

(defun deliver-message-if-deliverable (msg)
  (when (mail-message-deliverable-p msg)
    (deliver-mail-message msg)
    (pull msg *mail-delivery-hold*)
    t))

;; make mnormal mail delivery toplevel and held msg delivery toplevel two separate threads

(defun mail-delivery-toplevel ()
  (unwind-protect
      (loop for work = (or (maybe-deliver-some-held-messages)
			   (and (not (queue-empty-p *mail-delivery-active-queue*))
				(dequeue *mail-delivery-active-queue* :wait nil))
			   (if (zerop (held-message-count))
			       (dequeue *mail-delivery-active-queue* :wait t :whostate "Queue empty") ; Queue and Hold both empty: block.
			       (process-allow-schedule)))                                     ; Queue empty, Hold is not: allow-schedule.
	    do (when (mail-msg-out-p work)
		 (if (mail-message-deliverable-p work)
		     (deliver-mail-message work)
		     (progn (hold-mail-message work :host-busy 5)
			    (log-it :mail :hold "(~A) Holding message temporarily because stream is busy." (mail-msg-out-internal-id work))
))))))

(defun deliver-mail-message (msg)
  (let ((to (mail-msg-out-to msg)))
    (register-mail-delivery-thread 
     to   
     (process-run-function `(:name , (format nil "Delivering mail to ~A" to)
				     :initial-bindings ((*package* . ,(find-package 'tits))
							,@excl:*cl-default-special-bindings*))
			   #'deliver-mail-msg-toplevel
			   msg))))

; hmm i could check if there are any more messages for this recipient in Hold after the first msg is sent,
; to avoid making new threads every msg.
(defun deliver-mail-msg-toplevel (mail-msg) 
  (let* ((mailto (mail-msg-out-to mail-msg))
	 (smtp (ensure-smtp-stream mailto)))
    (unwind-protect
	(write-email-smtp smtp mail-msg)
      (unregister-mail-delivery-thread mailto))))

;(defun ensure-smtp-stream (mailto) ; this function is contorted and probably broken.
;;; don't make a new smtp-stream unless all the other smtp-streams open to the host are busy.
;;; hmm so the structure of the "host->smtp-stream-table" should actually be host->(stream1 ... streamn)
;;; need to have something that periodically kills streams that are at EOF.
;  (let* ((host (frob::email-address-host mailto))
;	 (smtp (gethash host *host->smtp-stream-table*)))
;    (unless (and smtp (open-stream-p smtp))
;      ;;-hmm.. make sure this doesn't screw us when a socket is closed but stream is open.  Use LISTEN to determine if theres an EOF.
;      (setf smtp (smtp-client::initialize-smtp-stream host)))
;    (unless smtp
;      (setf (gethash host *host->smtp-stream-table*) smtp))
;    smtp))

(defun write-email-smtp (smtp-stream msg)
  (with-slots (from to subject body) msg
    (let* ((max-size (transport-message-size-limit (tty-transport (mail-msg-out-tty msg))))
	   (msg-size (length body))
	   (fragment-count (ceiling msg-size max-size)))
      (loop for fragment-num from 1 to fragment-count
	    as fragment = (subseq body (* max-size (1- fragment-num)) (* (min msg-size max-size) fragment-num)) ; make use SUBSEQUENCE stuff
	    as id = (generate-message-id msg fragment-num fragment-count)
	    with start-time = (get-universal-time)
	    do (progn (log-it :mail :delivery "(~A) ~A Delivery started, message had been waiting for ~D seconds."
			      (mail-msg-out-internal-id msg) id (- start-time (mail-msg-out-insertion-time msg)))
		      ;; do I have to re-write the envelope for each fragment? check rfc821
		      (smtp-client::write-smtp-envelope smtp-stream from to) 
		      (smtp-client::cmd-data smtp-stream from to subject id fragment)
		      (log-it :mail :delivery "(~A) ~A Finished delivering message.  SMTP dialogue took ~D seconds."
			      (mail-msg-out-internal-id msg) id (- (get-universal-time) start-time)))))))

(defun handle-smtp-reply-code (msg code)
  "Handle SMTP reply codes for normal commands (everything except DATA)."
  (multiple-value-bind (one two three)
      (smtp-reply-codes::numeric-reply-code-to-symbolic code)
    (declare (ignore two three))
    (cond ((eql one :positive-preliminary)          ; dump-dialogue, abandon delivery.  before totally abandoning, try other MX-RR's.
	   ())
	  ((eql one :positive-completion))           ; be happy
	  ((eql one :positive-intermediate))         ; dump-dialogue, abandon delivery
	  ((eql one :transient-negative-completion)  ; retry, log
;	   (log-it :mail "While attempting delivery of ~A, got reply code ~D: ~S.  Will retry in ~D seconds."
;	   internal-id code (list one two three) retry-in)
	   (hold-mail-message msg :transient-delivery-problem (* 60 1)))
	  ((eql one :permanent-negative-completion) ; retry later, log
;	   (log-it :mail "While attempting delivery of ~A, got reply code ~D: ~S.  Will retry in ~D seconds." 
;	   internal-id code (list one two three) retry-in)
	   (hold-mail-message msg :delivery-problem (* 60 5))) ; hold for at least 5 minutes -- this should increment as problem persists
	  ((eql one :unspecified)))))                ; dump-dialogue, abandon delivery

; <221572381.gz-249-32471.2of5.zippy@trancell.com>
(defun generate-message-id (msg fragment-number fragment-count)
  "<UT.SHORT-HOSTNAME.COLD-RESET-COUNT.MESSAGE-NUMBER.FRAGMENT-NUMBERofFRAGMENT-COUNT.USER@trancell.com>"
  (with-slots (tty message-number message-id) msg
    (setf message-id
      (format nil "<~D.~A-~D-~D.~Dof~D.~A@~A>" (get-universal-time) *short-host-name* 
	      *cold-reset-count* message-number fragment-number fragment-count
	      (user-uname (tty-owner tty)) *our-mailhost-name*))))
|#
