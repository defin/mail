(in-package cl-user)

(defpackage mail-base
  (:documentation "Basic mail functionality.")
  (:use cl excl rutils)
  (:export find-header-class unknown-header date-time mailbox msgid parse-mail))

(defpackage rfc2822
  ;; mail-base should not be a nickname of this,, should be aseperate package rfc2822 and mime inherit from
  (:nicknames rfc2822-base rfc2822-parser)
  (:documentation "RFC2822-specific functionality.")
  (:use cl excl rutils mail-base)
  (:export rfc2822-message rfc2822-header assemble-rfc2822-message))

(defpackage mime
  (:use cl excl rutils mail-base)
  (:documentation "MIME-specific functionality."))


