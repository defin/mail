(in-package mail-process)

;;; store templates for a variety of canned replies.. templates should allow inline string substitution in headers and body, like |foo|
;;; should be an extension of the templates already used by generator routines

(defclass canned-reply ()
  ())

(defclass reply-order-shipped (canned-reply) ; each of these could have multiple subclasses with different templates
  ())

(defclass reply-order-delayed (canned-reply)
  ())

(defclass reply-wait-for-shipment-status (canned-reply)
  ())
