(in-package rfc2822)

;;; Message-ID lists

(defclass msgid ()
  ((id :accessor msgid-id)
   (original :initarg :original
	     :accessor msgid-original)))

(defun parse-message-id-list (string)
  (loop named chop 
	with msg-ids = nil
	with input = (subseq string 0)
	with done = nil
	as position = (position " " input :test #'string=)
	until done
	do (if position
	       (progn (push (subseq input 0 position) msg-ids)
		      (setf input (subseq input (1+ position))))
	       (progn (push input msg-ids)
		      (setf done t)))
	finally (return-from chop (reverse msg-ids))))

