(in-package mail-inet)

(defmacro with-pop3-mailbox ((host user password &optional (message-count-var 'message-count)) &body body)
  (let ((mailbox (gensym "MAILBOX-"))
	(retval (gensym "RETVAL-")))
    `(let* ((,mailbox (net.post-office:make-pop-connection ,host :user ,user :password ,password))
	    (,message-count-var (net.post-office:mailbox-message-count ,mailbox))
	    (,retval nil))
      (unwind-protect
	   (flet ((retrieve-message (msgnum)
		    (net.post-office:fetch-letter ,mailbox msgnum))
		  (delete-message (msgnum)
		    (net.post-office:delete-letter ,mailbox msgnum))
		  (top-lines (msgnum &optional (count 0))
		    (net.post-office:top-lines ,mailbox msgnum count))
		  (noop ()
		    (net.post-office:noop ,mailbox))
		  (unique-id (&optional (message nil))
		    (net.post-office:unique-id ,mailbox message))
		  (reset-mailbox ()
		    (net.post-office:reset-mailbox ,mailbox)))
	     (setf ,retval (multiple-value-list (progn ,@body))))
	(net.post-office:close-connection ,mailbox))
      (values-list ,retval))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-pop3-mailbox retrieve-message delete-message top-lines noop unique-id reset-mailbox)))


