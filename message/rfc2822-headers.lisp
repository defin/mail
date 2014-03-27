(in-package rfc2822)

(defclass rfc2822-header (header)
  ()
  (:documentation "Standard header defined in RFC2822."))

(define-header-class ("From" rfc2822-header)
  ((mailboxes :initform nil
	      :type list
	      :accessor header-mailboxes)))

(defmethod parse-header ((header from-header))
  (with-slots (mailboxes content) header
    (setf mailboxes (parse-mailbox-list content))))

(defmethod value-for-header ((hdr from-header))
  (mapcar #'mailbox-original (header-mailboxes hdr)))

; anyone use this?
(defmethod message-from ((message rfc2822-message))
  (let ((hdr (find-header 'from-header (rfc2822-message-headers message))))
    (if hdr
	(header-mailboxes hdr)
	nil)))

(defmethod message-from-mailbox ((message rfc2822-message))
  (first (message-from message)))

;(defmethod (setf message-from-mailbox) ((message rfc2822-message))
;  (setf (message-from ) nil))

(defmethod message-from-address ((message rfc2822-message))
  (mailbox-address (message-from-mailbox message)))

(define-header-class ("Date" rfc2822-header)
  ((date-time :initarg :date-time
	      :accessor header-date-time)
   (ut :accessor header-ut
       :initarg :ut)))

(defmethod parse-header ((header date-header))
  (if (slot-boundp header 'content)
      (progn (setf (header-date-time header) (parse-date-time (make-instance 'date-time :original (header-content header))))
	     (setf (header-ut header) (date-time-ut (header-date-time header))))
      (setf (header-date-time header) (make-instance 'date-time :ut (header-ut header)))))

(defmethod value-for-header ((hdr date-header))
  (header-ut hdr))

; anyone use this?
(defmethod message-date ((message rfc2822-message))
  (let ((hdr (find-header 'date-header (rfc2822-message-headers message))))
    (if hdr
	(header-ut hdr)
	nil)))

(defmethod message-date-ut ((message rfc2822-message))
  (with-slots (headers) message
    (aif (find-header 'date-header headers)
	 (header-ut it)
	 0)))

;defined in parser
;(defmethod message-date ((message rfc2822-message))
;  (with-slots (headers) message
;    (awhen (find-header 'date-header headers) 
;	   (header-content it))))

(define-header-class ("To" rfc2822-header)
  ((mailboxes :initform nil
	      :initarg :mailboxes
	      :type list
	      :accessor header-mailboxes)))

(defmethod parse-header ((header to-header))
  (with-slots (mailboxes content) header
    (setf mailboxes (parse-mailbox-list content))))

(defmethod value-for-header ((hdr to-header))
  (mapcar #'mailbox-original (header-mailboxes hdr)))

; anyone use this?
(defmethod message-to ((message rfc2822-message))
  (let ((hdr (find-header 'to-header (rfc2822-message-headers message))))
    (if hdr
	(header-mailboxes hdr)
	nil)))

(defmethod message-to-mailbox ((message rfc2822-message))
  (first (message-to message)))

(defmethod message-to-address ((message rfc2822-message))
  (mailbox-address (message-to-mailbox message)))

(define-header-class ("Sender" rfc2822-header)
  ((mailbox :accessor header-mailbox)))

(define-header-class ("Received" rfc2822-header)
  ((from :accessor header-from)
   (by :accessor header-by)
   (via :accessor header-via)
   (with :accessor header-with)
   (id :accessor header-id)
   (for :accessor header-for)
   (time :accessor header-time)))

(define-header-class ("Subject" rfc2822-header)
  ()) ; a subject header is just its content, no special parse possible

(defmethod parse-header ((header subject-header)) 
  t)

(defmethod value-for-header ((hdr subject-header))
  (header-content hdr))

; anyone use this?
(defmethod message-subject ((message rfc2822-message)) ; no parse of a subject header is possible, just return content
  (let ((hdr (find-header 'subject-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

;defined in parser
;(defmethod message-subject ((message rfc2822-message))
;  (with-slots (headers) message
;    (awhen (find-header 'subject-header headers) 
;	   (header-content it))))

(define-header-class ("In-Reply-To" rfc2822-header)
  ((message-ids :accessor header-message-ids
		:type list)))

(defmethod parse-header ((header in-reply-to-header))
  (with-slots (message-ids content) header  
    (setf message-ids (parse-message-id-list content))))

(defmethod value-for-header ((hdr in-reply-to-header))
  (header-message-ids hdr))

; anyone use this?
(defmethod message-in-reply-to ((message rfc2822-message))
  (let ((hdr (find-header 'in-reply-to-header (rfc2822-message-headers message))))
    (if hdr
	(header-message-ids hdr)
	nil)))

;defined in parser
;(defmethod message-in-reply-to ((message rfc2822-message))
;  (with-slots (headers) message
;    (awhen (find-header 'in-reply-to-header headers)
;	   (header-message-ids it))))

(define-header-class ("References" rfc2822-header)
  ((message-ids :accessor header-message-ids
		:type list)))

(defmethod parse-header ((header references-header))
  (with-slots (message-ids content) header
    (setf message-ids (parse-message-id-list content))))

(defmethod value-for-header ((hdr references-header))
  (header-message-ids hdr))

; anyone use this?
(defmethod message-references ((message rfc2822-message))
  (let ((hdr (find-header 'references-header (rfc2822-message-headers message))))
    (if hdr
	(header-message-ids hdr)
	nil)))

;defined in parser
;(defmethod message-references ((message rfc2822-message))
;  (with-slots (headers) message
;    (awhen (find-header 'references-header headers) 
;	   (header-message-ids it))))

(define-header-class ("Message-ID" rfc2822-header)
  ((message-id :accessor header-message-id)))

(defmethod parse-header ((header message-id-header))
  (with-slots (message-id content) header
    (setf message-id (first (parse-message-id-list content)))))

(defmethod value-for-header ((hdr message-id-header))
  (header-message-id hdr))

; anyone use this?
(defmethod message-message-id ((message rfc2822-message))
  (let ((hdr (find-header 'message-id-header (rfc2822-message-headers message))))
    (if hdr
	(header-message-id hdr)
	nil)))

;;defined in parser
;(defmethod message-message-id ((message rfc2822-message))
;  (with-slots (headers) message
;    (awhen (find-header 'message-id-header headers) 
;	   (header-content it))))

(define-header-class ("Cc" rfc2822-header)
  ((mailboxes :initform nil
	      :type list
	      :accessor header-mailboxes)))

(defmethod parse-header ((header cc-header))
  (with-slots (mailboxes content) header
    (setf mailboxes (parse-mailbox-list content))))

(defmethod value-for-header ((hdr cc-header))
  (mapcar #'mailbox-original (header-mailboxes hdr)))

; anyone use this?
(defmethod message-cc ((message rfc2822-message))
  (let ((hdr (find-header 'cc-header (rfc2822-message-headers message))))
    (if hdr
	(header-mailboxes hdr)
	nil)))

(defmethod message-cc-mailbox ((message rfc2822-message))
  (first (message-cc message)))

(define-header-class ("Reply-To" rfc2822-header)
  ((mailbox :initform nil
	    :accessor header-mailbox)))

(defmethod parse-header ((header reply-to-header))
  (with-slots (mailbox content) header
    (setf mailbox (parse-mailbox (make-instance 'mailbox :original content)))))

(defmethod value-for-header ((hdr reply-to-header))
  (mailbox-original (header-mailbox hdr)))

; anyone use this?
(defmethod message-reply-to ((message rfc2822-message))
  (let ((hdr (find-header 'reply-to-header (rfc2822-message-headers message))))
    (if hdr
	(header-mailbox hdr)
	nil)))

;;; Unimportant standard headers, just return content

(define-header-class ("Return-Path" rfc2822-header)
  ())

(defmethod parse-header ((header return-path-header)) 
  t)

; anyone use this?
(defmethod message-return-path ((message rfc2822-message))
  (let ((hdr (find-header 'return-path-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-Reply-To" rfc2822-header)
  ())

(defmethod parse-header ((header resent-reply-to-header)) 
  t)

; anyone use this?
(defmethod message-resent-reply-to ((message rfc2822-message))
  (let ((hdr (find-header 'resent-reply-to-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-From" rfc2822-header)
  ())

(defmethod parse-header ((header resent-from-header)) 
  t)

; anyone use this?
(defmethod message-resent-from ((message rfc2822-message))
  (let ((hdr (find-header 'resent-from-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-Sender" rfc2822-header)
  ())

(defmethod parse-header ((header resent-sender-header))
  t)

; anyone use this?
(defmethod message-resent-sender ((message rfc2822-message))
  (let ((hdr (find-header 'resent-sender-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-Date" rfc2822-header)
  ())

(defmethod parse-header ((header resent-date-header))
  t)

; anyone use this?
(defmethod message-resent-date ((message rfc2822-message))
  (let ((hdr (find-header 'resent-date-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-To" rfc2822-header)
  ())

(defmethod parse-header ((header resent-to-header))
  t)

; anyone use this?
(defmethod message-resent-to ((message rfc2822-message))
  (let ((hdr (find-header 'resent-to-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-Cc" rfc2822-header)
  ())

(defmethod parse-header ((header resent-cc-header))
  t)

; anyone use this?
(defmethod message-resent-cc ((message rfc2822-message))
  (let ((hdr (find-header 'resent-cc-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-Bcc" rfc2822-header)
  ())

(defmethod parse-header ((header resent-bcc-header))
  t)

; anyone use this?
(defmethod message-resent-bcc ((message rfc2822-message))
  (let ((hdr (find-header 'resent-bcc-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Resent-Message-ID" rfc2822-header)
  ())

(defmethod parse-header ((header resent-message-id-header))
  t)

; anyone use this?
(defmethod message-resent-message-id ((message rfc2822-message))
  (let ((hdr (find-header 'resent-message-id-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Bcc" rfc2822-header)
  ())

(defmethod parse-header ((header bcc-header))
  t)

; anyone use this?
(defmethod message-bcc ((message rfc2822-message))
  (let ((hdr (find-header 'bcc-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Keywords" rfc2822-header)
  ())

(defmethod parse-header ((header keywords-header)) ; just a space seperated list, right?
  t)

; anyone use this?
(defmethod message-keywords ((message rfc2822-message))
  (let ((hdr (find-header 'keywords-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Comments" rfc2822-header)
  ())

(defmethod parse-header ((header comments-header))
  t)

; anyone use this?
(defmethod message-comments ((message rfc2822-message))
  (let ((hdr (find-header 'comments-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(define-header-class ("Encrypted" rfc2822-header)
  ())

(defmethod parse-header ((header encrypted-header))
  t)

; anyone use this?
(defmethod message-encrypted ((message rfc2822-message))
  (let ((hdr (find-header 'encrypted-header (rfc2822-message-headers message))))
    (if hdr
	(header-content hdr)
	nil)))

(export-all-symbols)

