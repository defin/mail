(in-package rfc2822)

; ! be sure to fold long header lines!
(defmethod print-header ((header header) stream)
  (let* ((pretty-name (header-pretty-name header))
	 (unfolded-string (format nil "~A: ~A" pretty-name (header-content header))))
    (write-sequence (fold-header-line unfolded-string) stream)))

(defun fold-header-line (string)
  string)

(defmethod print-message ((message rfc2822-message)) ; map over headers printing values of each ORIGINAL slot, print crlfcrlf, print body
  (concatenate 'string 
	       (message-headers-string message)
	       *crlf-crlf*
	       (rfc2822-message-body message)))

(defmethod message-headers-string ((message rfc2822-message))
  (with-output-to-string (s)
    (let ((headers (rfc2822-message-headers message)))
      (map nil #'(lambda (h)
		   (when (slot-boundp h 'rfc2822::content)
		     (unless (string= (header-content h) "")
		       (fresh-line s)
		       (print-header h s))))
	   headers))))

;;; Generate a rfc822 message from a template and some parameters

(defclass template ()
  ((body)))

(defclass bookmanager-template (template)
  ((from) ; these should be header instances
   (to)
   (reply-to)))

(defmethod assemble-rfc2822-message ((template template))
  )
