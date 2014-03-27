(defpackage rfc822
  (:use cl))

(in-package rfc822)

(defun make-syntax-rule-name (name)
  (intern (format nil "SYNTAX-RULE/~A" (string-upcase name))))

(defmacro trying-syntax-rule ((name start) &body body)
  (let ((fname ,(gensym)))
    `(let ((,fname (make-syntax-rule-name ,name)))
       (multiple-value-bind (match lastpos)
	   (funcall #',fname ,start)
         ,@body))))

(defmacro define-syntax-rule (name documentation &body body)
  (let ((fname (gensym)))
    `(let ((,fname (make-syntax-rule-name ,name)))
       (defun ,fname (string &optional (start 0)) 
	 ,documentation
	 ,@body))))

;;; Definitions of mail syntax rules, from RFC822, Appendix D (?), with updates from RFC1123, Section 4 (?)

(define-syntax-rule address
  "mailbox / group"
  (trying-syntax-rule (mailbox start)
    (when match (values match lastpos)))
  (trying-syntax-rule (group start)
    (when match (values match lastpos)))
  (values nil start))

(define-syntax-rule addr-spec
  "local-part \"@\" domain"
  (trying-syntax-rule (local-part start)
    (when match
      (let ((local-part match))
	(when (char= (aref string (1+ lastpos)) #\@) ; 1+ here? need to make sure everything is consistent wrt who increments this for what
	  (trying-syntax-rule (domain (+ 2 lastpos)) ; UHH CANT THERE BE A SPACE HERE LEGALLY ACCORDING TO RFC822?
	    (when match
	      (values (concatenate 'string local-part "@" match) lastpos))))))
    (values nil start)))

(define-syntax-rule atom
  "1*<any CHAR except specials, SPACE and CTLs>"
  (let ((endpos (loop named hmm
		      with pos = start
		      do (cond ((or (char= (aref string pos) #\space) 
				    (< (char-code (aref string pos)) 32)) ; is this the right place for this?
				(return-from hmm pos))
			       ;; just rip this out of SYNTAX-RULE/SPECIALS for now cause im lazy. also not using CHAR rule here
			       ((member char '(#\( #\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\. #\[ #\]))
				(return-from hmm nil))
			       (t (incf pos))))))
    (if (or (null endpos) (= endpos start))
	(values nil start)
        (values (subseq start endpos) endpos)))) ; 1+ endpos ?

(define-syntax-rule char
  "<any ASCII character, 0-127>"
  (let ((char (aref string start)))
  (if (< (char-code char) 128)
      (values char (1+ start))
      (values nil start))))

(define-syntax-rule comment
  "\"(\" *(ctext / quoted-pair / comment) \")\"")

(define-syntax-rule ctext
  "<any CHAR excluding (, ), \\ & CR & including LWSP>")

(define-syntax-rule date
  "1*2DIGIT month 2*4DIGIT ; from RFC1123")

(define-syntax-rule date-time
  "[ day "," ] date time")

(define-syntax-rule day
  "Mon / Tue / Wed / Thu / Fri / Sat / Sun")

(define-syntax-rule domain-literal
  "\"[\" *(dtext / quoted-pair) \"]\"")

(define-syntax-rule domain
  "sub-domain *(\".\" sub-domain)")

(define-syntax-rule dtext 
  "<any char excluding [, ], \\ & CR & including LWSP>"
  (trying-syntax-rule (char start)
    (if (and match (not (member match '(#\[ #\] #\\ #\^m) :test #'char=)))
	(values match lastpos)
        (values nil start))))

(define-syntax-rule field
  "field-name \":\" [ field-body ] CRLF")

(define-syntax-rule field-body 
  "field-body-contents [CRLF LWSP-char field-body]")

(define-syntax-rule field-name
  "1*<any CHAR, excluding CTLs, SPACE, and \":\"")

(define-syntax-rule group 
  "phrase \":\" [#mailbox] \";\"")

(define-syntax-rule hour
  "2DIGIT \":\" 2DIGIT [\":\" 2DIGIT]")

(define-syntax-rule local-part
  "word *(\".\" word)")

(define-syntax-rule LWSP
  "1*([CRLF] LWSP-char)")

(define-syntax-rule LWSP-char
  "SPACE / HTAB")

(define-syntax-rule mailbox 
  "addr-spec / [phrase] route-addr ; from RFC1123"
  (trying-syntax-rule (addr-spec start)                          ; addr-spec case
    (when match (values match lastpos)))
  (trying-syntax-rule (phrase start)                             ; phrase route-addr case
    (when match
      (let ((mailbox (list match)))
	(trying-syntax-rule (route-addr lastpos)
	  (when match
	    (values (nreverse (push match mailbox)) lastpos))))))
  (trying-syntax-rule (route-addr start)                         ; route-addr case
    (when match (values match lastpos)))
  (values nil start))

(define-syntax-rule month
  "Jan / Feb / Mar / Apr / May / Jun / Jul / Aug / Sep / Oct / Nov / Dec")

(define-syntax-rule msg-id
  "\"<\" addr-spec \">\"")

(define-syntax-rule orig-date
  "\"Date\" \":\" date-time")

(define-syntax-rule phrase 
  "1*word")

(define-syntax-rule qtext
  "<any char excluding \", \\ & CR, and including LWSP>"
  (trying-syntax-rule (char start)
    (if (and match (not (member match '(#\" #\\ #\^m) :test #'char=)))
	(values match lastpos)
        (values nil start))))

(define-syntax-rule quoted-pair 
  "\\ char"
  (if (char= (aref string start) #\\)
      (trying-syntax-rule (char (1+ start))
	(if match
	    (values match lastpos)
	    (values nil start)))
      (values nil start)))

(define-syntax-rule quoted-string 
  "<\"> *(qtext/quoted-pair) <\">"
  (if (char= (aref string start) #\")
      (let ((endpos (loop named hmm
			  with pos = (1+ start)
			  do (cond ((char= (aref string pos) #\")
				    (return-from hmm pos))
				   ((trying-syntax-rule (qtext pos)
				      (when match 
					(incf pos lastpos))))
				   ((trying-syntax-rule (quoted-pair pos)
				      (when match
					(incf pos lastpos))))
				   (t (return-from hmm nil))))))
	(if endpos
	    (values (subseq string (1+ start) (1- endpos)) endpos)
	    (values nil endpos)))
      (values nil start)))

(define-syntax-rule received
  "\"Received\" \":\" [\"from\" domain] [\"by\" domain] [\"via\" atom] *(\"with\" atom) [\"id\" msg-id] [\"for\" addr-spec] \";\" date-time"
)

(define-syntax-rule return
  "route-addr / \"<\" \">\" ; from RFC1123")

(define-syntax-rule route 
  "1#(\"@\" domain) \":\" ; deprecated by RFC1123")

(define-syntax-rule route-addr 
  "\"<\" [route] addr-spec \">\"")

(define-syntax-rule specials
  "( / ) / < / > / @ / , / ; / : / \\ / \" / . / [ / ]"
  (let ((char (aref string start)))
    (if (member char '(#\( #\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\. #\[ #\]))
	(Values char (1+ start))
        (values nil start))))

(define-syntax-rule sub-domain
  "domain-ref / domain-literal"
;  (trying-syntax-rule (atom start)              ; domain-ref = atom
;    (when match (values match lastpos)))
;  (trying-syntax-rule (domain-literal start)
;    (when match (values match lastpos)))
;  (values nil start))
  (syntax-or atom domain-literal))

(define-syntax-rule time
  "hour zone")

(define-syntax-rule word 
  "atom / quoted-string"
  (trying-syntax-rule (atom start)
    (when match (values match lastpos)))
  (trying-syntax-rule (quoted-string start)
    (when match (values match lastpos)))
  (values nil start))

(define-syntax-rule zone
  "UT / GMT / EST / EDT / CST / CDT / MST / MDT / PST / PDT / 1ALPHA / ( (\"+\" / \"-\") 4DIGIT ) ; from RFC1123")

;;; -><-


;;; -><-
