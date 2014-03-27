(in-package mail-process)

;;; read in a message, store it to mail archive, parse it, store it to main db, categorize it, and if applicable update database tables with changes

(defun process-message (message-text)
  (let* ((archive (mail-store:archive-message message-text))
	 (parsed (rfc2822-parser:make-rfc2822-message (mail-store:archive-message-headers archive)
						      (mail-store:archive-message-body archive)))
	 (msg (mail-store:create-message archive parsed))
					; (category (categorize-message msg parsed))
	 )
;    (apply-updates category msg)
    t))

#|
(setq am (mail-store:archive-message (with-pop3-mailbox ("10.5.5.32" "support" "B@heren0w" message-count) (retrieve-message 1))))
(setq pm (rfc2822-parser:make-rfc2822-message (mail-store:archive-message-headers am)
					      (mail-store:archive-message-body am)))
(setq msg (mail-store:create-message am pm))
|#
