(in-package mail-inet)

(defparameter *default-mail-proxy* (getf bs:*pop3-defaults* 'bs::*default-mail-proxy*))

(defstruct (pop3-mailbox) host user password)

(logger:define-log-message :checking-mail (:type :mail :severity :info)
  (user host)
  "Checking mail for ~A@~A."
  user host)

(logger:define-log-message :fetched-mail (:type :mail :severity :info)
  (count user host)
  "Retrieved ~D message~P for ~A@~A."
  count count user host)

(defparameter *pop3-mailbox-list* (mapcar #'(lambda (mailbox-desc)
					      (make-pop3-mailbox :user (getf mailbox-desc :user)
								 :password (getf mailbox-desc :password)
								 :host (or (getf mailbox-desc :host nil)
									   *default-mail-proxy*)))
					  (getf bs:*pop3-defaults* 'bs::*pop3-mailbox-list*)))

(logger:define-log-message :pop3-error (:type :mail :severity :fatal)
  (process report backtrace)
  "A POP3 error has occured in process ~A with report:~%~A~%Backtrace: ~A"
  process report backtrace)

(defun fetch-mail ()
  (dolist (m *pop3-mailbox-list*)
    (fetch-mail-from-mailbox m)))

(defun fetch-mail-from-mailbox (m)
  (logger:!log :checking-mail (pop3-mailbox-user m) (pop3-mailbox-host m))
  (let ((count (handler-case (check-mail-pop3 m)
		 (net.post-office:po-error (c)
		   (logger:log-error c :message-symbol :pop3-error :print-type nil)
		   0))))
    (when (plusp count)
      (logger:!log :fetched-mail count (pop3-mailbox-user m) (pop3-mailbox-host m)))))

(app:define-system-timer :fetch-mail
    (:name "Fetch Mail"
     :period #.(* 1 60)
     :target-facility :mail-backend
     :function fetch-mail))

;;; Mail Receipt

(defun check-mail-pop3 (pop3-mailbox)
  (with-slots (host user password) pop3-mailbox
    (with-pop3-mailbox (host user password message-count)
      (loop for msgnum from 1 to message-count
	    as message = (retrieve-message msgnum)
	    sum 1
	    if (and message (mail-process::process-message message))
	      do (delete-message msgnum)
	    else do (cerror "Failed to process message ~D" msgnum msgnum)
		    (Sleep 1)))))

(defun check-mail-mbox (mbox-file)
  (with-open-file (m mbox-file :direction :input)
    (loop with msg = nil
	  with endp = nil
	  do (multiple-value-setq (msg endp) (read-mbox-message m))
	  until endp
	  do (and msg (mail-process::process-message msg)))))

;(check-mail-mbox "z:/devo/code/sellbooks/data/bookmail3")




