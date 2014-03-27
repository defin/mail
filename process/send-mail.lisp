(in-package mail-process)

(defmethod rfc2822:assemble-rfc2822-message ((message t))
  (error "I don't know how to assemble an RFC2822 message out of object ~S." message))

(defmethod rfc2822:assemble-rfc2822-message ((message rfc2822-message))
  message)

(defmethod send-message ((message string))
  (send-message (assemble-rfc2822-message message)))

(defmethod send-message ((message rfc2822-message))
  (let* ((data (rfc2822::print-message message))
	 (fromaddr (mailbox-original (car (rfc2822::message-from message))))
	 (to (car (rfc2822::message-to message)))
	 (cc (car (rfc2822::message-cc message)))
	 (bcc (car (rfc2822::message-bcc message)))
	 (message (mail-store:create-message (mail-store:archive-message data) message :outgoing-p t)))
    (flet ((server (mailbox)
	     (second (multiple-value-list (rfc2822::parse-address (rfc2822::mailbox-original mailbox))))))
      (dolist (m (list to cc bcc))
	(when m
	  (let ((toaddr (rfc2822-base:mailbox-original m)))
	    (when (and toaddr (not (string= toaddr "")))
	      (mail-store:create-deliverable-message :host (server m)
						     :protocol "SMTP"
						     :envelope-from fromaddr
						     :envelope-to toaddr
						     :data data
						     :flags "queued"
						     :message-ptr (db-base:id message)))))))
    message))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(send-message assemble-rfc2822-message assign-message-id-header)))
