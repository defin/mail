;;; -*- Package: SMTP-CLIENT -*-

(defpackage SMTP-CLIENT
  (:use cl frob)
  (:export transact-smtp cmd-data cmd-rset write-smtp-message)
  (:import-from paul-graham acond it)
  (:import-from multiprocessing with-timeout wait-for-input-available))

(in-package smtp-client)

(defvar *smtp-port* 25)

(defmemoize-expiring mx-rr-lookup (host)
  #+allegro (multiple-value-bind (result valid-for other-results)
		(socket:dns-query host :type :mx)
	      (values (if (minusp valid-for)
			  0
			  valid-for)
		      result
		      other-results)))

; make sure this is fully compliant with RFC974 .. in the meantime we can punt messages to sendmail to deal with in complex cases.
(defun mx-lookup (host)
  (let ((mx (mx-rr-lookup host)))
    (if mx ; according to the MX rfc, this needs to be more complex to handle all cases.
	(acond ((second mx) it) ; If there is a pre-resolved ipaddr in the second slot use it
	       ((first mx) it)) ; Otherwise use the name in the first slot
        host)))

(defun read-smtp-response (stream)
  (loop named resp
	for line = (progn (wait-for-input-available stream) (read-line stream t)) ; need to do something with signalled error
	with text = nil
	do (if (char= (aref line 3) #\-)
	       (push (cdr (token-pair line :delimiter #\-)) text)
	       (let ((lastline (token-pair line :delimiter #\space)))
		 (push (cdr lastline) text)
		 (return-from resp (values (parse-integer (car lastline)) (nreverse text)))))))

(defmacro transact-smtp ((smtp-stream &optional (expected-reply-code 250) &key (timeout 600)) 
			 (format-control &rest format-args)
			 &optional (then-form nil) (else-form nil) (timeout-form nil))
  (let ((interpreted-code (gensym)))
    `(progn
       (if ,format-args
	   (format ,smtp-stream ,format-control ,@format-args)
	   (princ ,format-control ,smtp-stream))
       (force-output ,smtp-stream)
       (multiple-value-bind (.reply-code. .reply-text.)
	   (with-timeout (,timeout ,timeout-form)
	     (read-smtp-response ,smtp-stream))
	 (if (etypecase ,expected-reply-code
	       (number (= .reply-code. ,expected-reply-code))
	       (list (let ((,interpreted-code (numeric-reply-code-to-symbolic .reply-code. t)))
		       (ecase (length ,expected-reply-code)
			 (1 (equalp (first ,expected-reply-code) (first ,interpreted-code)))
			 (2 (and (equalp (first ,expected-reply-code) (first ,interpreted-code))
				 (equalp (second ,expected-reply-code) (third ,interpreted-code))))
			 (3 (equalp ,expected-reply-code ,interpreted-code)))))
	       (symbol (let ((,interpreted-code (numeric-reply-code-to-symbolic .reply-code. nil)))
			 (equalp ,expected-reply-code (first ,interpreted-code)))))
	     ,then-form
	     ,else-form)
	 (values .reply-code. .reply-text.)))))

(defvar *mail-host-history* (make-hash-table :test 'string-equal))

(defstruct (mail-host) host 220-line esmtp-options)

(defun initialize-smtp-stream (host &optional (port *smtp-port*))
  (let* ((mx (mx-lookup host))
	 (smtp (socket:make-socket :remote-host mx :remote-port port))
	 (mail-host (make-mail-host :host host)))
    (setf (stream-external-format smtp) :crlf-latin1-base)
    (multiple-value-bind (ignore 220-line)
        (with-timeout (600)
	  (read-smtp-response smtp))
      (setf (mail-host-220-line mail-host) 220-line))
    (transact-smtp (smtp 250) 
		   ("EHLO TRANCELL.COM~%")
		   (setf (mail-host-esmtp-options mail-host) (cdr .reply-text.))
		   (progn (transact-smtp (smtp 250) 
					 ("HELO TRANCELL.COM~%")
					 (setf (mail-host-esmtp-options mail-host) :smtp-only))))
    (maybe-record-mail-host host mail-host)
    smtp))

(defun maybe-record-mail-host (host mail-host)
  (let ((seen-before (gethash host *mail-host-history*)))
    (if seen-before
	(unless (equalp (mail-host-esmtp-options seen-before)
			(mail-host-esmtp-options mail-host))
	  (setf (gethash host *mail-host-history*) mail-host)
	  (when (boundp '*after-modify-mail-host-history-hooks*)
	    (run-hooks *after-modify-mail-host-history-hooks* mail-host)))
	(progn (setf (gethash host *mail-host-history*) mail-host)
	       (when (boundp '*after-add-mail-host-history-hooks*)
		 (run-hooks *after-add-mail-host-history-hooks* mail-host))))))

(defun cmd-rset (smtp-stream)
  (transact-smtp (smtp-stream) ("RSET~%")))

(defun cmd-mail-from (smtp-stream from) ; this needs to handle multiple froms
  (transact-smtp (smtp-stream) ("MAIL FROM:<~A>~%" from)))

(defun cmd-rcpt-to (smtp-stream to)  ; this needs to handle multiple tos
  (transact-smtp (smtp-stream) ("RCPT TO:<~A>~%" to)))

(defun cmd-data (smtp-stream from to subject message-id body) ; this shouldnt use transact-smtp at all
  (transact-smtp (smtp-stream 354) ("DATA~%"))
  (transact-smtp (smtp-stream 250) ((write-rfc822-message nil from to subject message-id body))))

(defun write-rfc822-message (smtp-stream mailfrom mailto subject message-id body)
  (prog1 (format smtp-stream "From: ~A~%~
                       To: ~A~%~
                       Subject: ~A~%~
                       Message-ID: ~A~%~
                       ~%~
                       ~A~%~
                       .~%" mailfrom mailto subject message-id body)
    (force-output smtp-stream)))

(defun write-smtp-envelope (smtp-stream mailfrom mailto) ; write a Pipelining version of this
  (cmd-rset smtp-stream)
  (cmd-mail-from smtp-stream mailfrom)
  (cmd-rcpt-to smtp-stream mailto))

(defun write-smtp-message (smtp-stream mailfrom mailto subject message-id body)
  (write-smtp-envelope smtp-stream mailfrom mailto)
  (cmd-data smtp-stream mailfrom mailto subject message-id body))

(defun send-mail-message (&key (from "nobody@nowhere.no") (to "president@whitehouse.gov") (subject "")
			       (message-id "") (body ""))
  (let* ((host (email-address-host to))
	 (smtp (initialize-smtp-stream host)))
    (write-smtp-message smtp from to subject message-id body)))
