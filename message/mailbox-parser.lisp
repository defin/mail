(in-package rfc2822)

#.(sharpslash:install)

;;; Mailbox parsing

(defclass mailbox ()
  ((local-part :initarg :local-part
	       :type string
	       :accessor mailbox-local-part
	       :documentation "The local part of the address.")
   (domain :initarg :domain
	   :type string
	   :accessor mailbox-domain
	   :documentation "The domain part of the address.")
   (phrase :initarg :phrase
	   :type string
	   :accessor mailbox-phrase
	   :documentation "The string associated with the mailbox, if any.")
   (original :initarg :original
	     :type string
	     :accessor mailbox-original
	     :documentation "The original text of the mailbox, including name and/or comments if any.")))

(defmethod mailbox-address ((mb mailbox))
  (list (mailbox-local-part mb)
	(mailbox-domain mb)
	(mailbox-phrase mb)))

(defconstant +address-regexp+ (compile-regexp #/"\(.+\)@\(.+\)") ; :case-fold nil :return :string :ignore-whitespace nil)
  "Regexp for matching email addresses, with submatches for local-part and domain.")

(defun parse-address (string)
  (if (search "@" string :test #'string=)
      (multiple-value-bind (success whole local domain)
	  (match-regexp +address-regexp+ string)
	(declare (ignore whole))
	(if success
	    (values local domain)
	    (error "this should never happen.")))
      (values string nil)))

(defconstant +angle-address-regexp+ (compile-regexp #/"\(.*\)\<\(.*\)\>\(.*\)")
  "Regexp for matching an address within angle brackets, with submatches for the part before the address, the address, and the part after.")

(defmethod parse-mailbox ((mailbox mailbox))
  (let ((address))
    (multiple-value-bind (not-comments comments)
	(remove-comments (mailbox-original mailbox))
      (multiple-value-bind (success whole before addr after)
	  (match-regexp +angle-address-regexp+ not-comments)
	(declare (ignore whole))
	(if success
	    (progn (setf (mailbox-phrase mailbox) 
			 (string-trim '(#\Space) (concatenate 'string 
							      (string-trim '(#\Space) before) " " (string-trim '(#\Space) after) " " comments)))
		   (setf address addr))
	    (progn (setf (mailbox-phrase mailbox) comments)
		   (setf address not-comments)))))
    (multiple-value-bind (local-part domain)
	(parse-address (string-trim '(#\Space) address))
      (setf (mailbox-local-part mailbox) local-part
	    (mailbox-domain mailbox) domain)))
    mailbox)

(defconstant +comma-list-regexp+ (compile-regexp #/"\([^,]+\),\(.+\)")
  "Regexp for breaking apart comma-seperated lists, with submatches for the first and rest parts of the list.")

(defun parse-mailbox-list (string)
  (loop named chop 
	with mailboxes = nil
	with input = (subseq string 0)
	with done = nil
	until done
	do (multiple-value-bind (success whole first rest)
	       (match-regexp +comma-list-regexp+ input)
	     (declare (ignore whole))
	     (if success
		 (progn (setf input (string-left-trim '(#\Space) rest))
			(push (parse-mailbox (make-instance 'mailbox :original first)) mailboxes))
		 (progn (push (parse-mailbox (make-instance 'mailbox :original input)) mailboxes)
			(setf done t))))
	finally (return-from chop (reverse mailboxes))))

