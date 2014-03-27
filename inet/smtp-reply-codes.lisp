;;; -*- Package: SMTP-REPLY-CODES -*-

(defpackage SMTP-REPLY-CODES
  (:use cl)
  (:export numeric-reply-code-to-symbolic #|symbolic-reply-code-to-numeric|#))

(in-package smtp-reply-codes)

(defun numeric-reply-code-to-symbolic (code &optional (thirdp nil))
  (let* ((digit1 (parse-integer (string (char code 0))))
	 (digit2 (parse-integer (string (char code 1))))
	 (digit3 (parse-integer (string (char code 2)))))
    (multiple-value-bind (symbol1 symbol2)
        (interpret-smtp-reply-position-12 digit1 digit2)
      (values symbol1 symbol2 (if thirdp (interpret-smtp-reply-position-3 symbol1 symbol2 digit3) nil)))))

(defun interpret-smtp-reply-position-12 (digit1 digit2)
  "Interpret the digits in positions 1 and 2 of the reply code, symbolically."
  (values 
   (case digit1
     (0 :unspecified)
     (1 :positive-preliminary)
     (2 :positive-completion)
     (3 :positive-intermediate)
     (4 :transient-negative-completion)
     (5 :permanent-negative-completion)
     (otherwise :unspecified))
   (case digit2
     (0 :syntax)
     (1 :information)
     (2 :connections)
     (3 :unspecified)
     (4 :unspecified)
     (5 :mail-system)
     (otherwise :unspecified))))

(defun interpret-smtp-reply-position-3 (symbol1 symbol2 digit3)
  "Interpret the digit in position 3 of the reply code, symbolically."
  (case symbol1
    (:positive-completion (case symbol2                                                 ; 2yz
			    (:information (case digit3                                  ; 21z
					    (1 :status)                                 ; 211
					    (4 :help)                                   ; 214
					    (otherwise :unspecified)))                  ; 21*
			    (:connections (case digit3                                  ; 22z
					    (0 :ready)                                  ; 220
					    (1 :closing-transmission-channel)           ; 221
					    (otherwise :unspecified)))                  ; 22*
			    (:mail-system (case digit3                                  ; 25z
					    (0 :ok)                                     ; 250
					    (1 :user-not-local)                         ; 251
					    (otherwise :unspecified)))                  ; 25*
			    (otherwise :unspecified)))                                  ; 2**
    (:positive-intermediate (case symbol2                                               ; 3yz
			      (:mail-system (case digit3                                ; 35z
					      (4 :start-mail-input)                     ; 354
					      (otherwise :unspecified)))                ; 35*
			      (otherwise :unspecified))                                 ; 3**
    (:transient-negative-completion (case symbol2                                       ; 4yz
				      (:connections (case digit3                        ; 42z
						      (1 :service-not-available)        ; 421
						      (otherwise :unspecified)))        ; 42*
				      (:mail-system (case digit3                        ; 45z
						       (0 :mailbox-unavailable)         ; 450
						       (1 :local-error)                 ; 451
						       (2 :insufficient-storage)        ; 452
						       (otherwise :unspecified)))       ; 45*
				      (otherwise :unspecified)))                        ; 4**
    (:permanent-negative-completion (case symbol2                                       ; 5yz
				      (:syntax (case digit3                             ; 50z
						 (0 :command-unrecognized)              ; 500
						 (1 :parameter-syntax-error)            ; 501
						 (2 :command-not-implemented)           ; 502
						 (3 :bad-command-sequence)              ; 503
						 (4 :command-parameter-not-implemented) ; 504
						 (otherwise :unspecified)))             ; 50*
				      (:mail-system (case digit3                        ; 55z
						      (0 :mailbox-unavailable)          ; 550
						      (1 :user-not-local)               ; 551
						      (2 :exceeded-storage-allocation)  ; 552
						      (3 :mailbox-name-not-allowed)     ; 553
						      (4 :transaction-failed)           ; 554
						      (otherwise :unspecified)))        ; 55*
				      (otherwise :unspecified)))                        ; 5**
    (:unspecified :unspecified)                                                         ; 0**
    (:positive-preliminary :unspecified)                                                ; 1**
    (otherwise :unspecified))))                                                         ; ***

