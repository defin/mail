(in-package mail-scraper)

(defclass mail-scraper (buffer-scraper)
  ((message :accessor scraper-message
	    :initarg :message)
   (message-ptr
    :accessor scraper-message-ptr
    :initarg :message-ptr
    :type integer
    :initform 0)))

(defclass mail-info (info)
  ())

(defun make-mail-scraper-internal (message class info-class parser-class message-ptr)
  (with-editor ()
    (let* ((buf (open-string (rfc2822:message-body message)))
	   (scraper (make-instance class
				   :message message
				   :message-ptr message-ptr
				   :editor *editor*
				   :buffer buf)))
      (setf (current-buffer) buf)
      (with-accessors ((info scraper-info)
		       (parser scraper-parser)
		       (state scraper-state)) scraper
	(setf info (make-instance info-class))
	(setf parser (make-instance parser-class))
	(setf state :ready))
      scraper)))

;; For testing purposes, not used in production
(defmethod make-mail-scraper ((mail pathname) (class t) &key (message-ptr 0) &allow-other-keys)
  (make-mail-scraper (file->string mail) class :message-ptr message-ptr))

(defmethod make-mail-scraper ((mail string) (class t) &key (message-ptr 0) &allow-other-keys)
  (multiple-value-bind (headers body)
      (rfc2822-parser:divide-entity-container mail)
    (make-mail-scraper (rfc2822-parser:make-rfc2822-message headers body) class :message-ptr message-ptr)))

;; Scrape database messages, the usual deal
(defmethod make-mail-scraper ((mail mail-store:message) (class t) &key &allow-other-keys)
  (let ((archive-message (mail-store:message-archive-message-ptr mail)))
    (make-mail-scraper archive-message class :message-ptr (db-base:id mail))))

(defmethod make-mail-scraper ((mail mail-store:archive-message) (class t) &key (message-ptr 0) &allow-other-keys)
  (let ((headers (mail-store:archive-message-headers mail))
	(body (mail-store:archive-message-body mail)))
    (make-mail-scraper (rfc2822-parser:make-rfc2822-message headers body) class :message-ptr message-ptr)))

;; Eventually the mail needs to be a rfc2822-message to get to work
(defmethod make-mail-scraper ((mail rfc2822:rfc2822-message) (class t) &key &allow-other-keys)
  (error "I don't know how to make a mail scraper of class ~A out of an RFC2822-MESSAGE." class))

(defmacro define-mail-scraper ((name) &rest info-slots)
  `(scraper-base::define-scraper-internal
     (,name
      :scraper-class mail-scraper
      :parser-class editor:editor
      :info-class mail-info
      :info-slots ,info-slots)
    (defmethod make-mail-scraper ((mail rfc2822:rfc2822-message) (class (eql ',name)) &key (message-ptr 0))
      (make-mail-scraper-internal mail ',name scraper-base::info-name scraper-base::parser-name message-ptr))))

(defmethod scrape :around ((scraper buffer-scraper))
  (with-editor ((scraper-editor scraper))
    (call-next-method)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mail-scraper mail-info define-mail-scraper scraper-message scraper-message-ptr make-mail-scraper)))
