(in-package rfc2822)

#.(sharpslash:install)

;;; Dates

(defclass date-time ()
  ((original :initarg :original
	     :accessor date-time-original
	     :type string)
   (comments :accessor date-time-comments
	     :type string)
   (ut :accessor date-time-ut
       :initarg :ut
       :type integer)))

(defconstant +month-table+ #(nil "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defconstant +zone-table+
  '(("UT"  .  0)
    ("GMT" .  0)
    ("EST" . -5)
    ("EDT" . -4)
    ("CST" . -6)
    ("CDT" . -5)
    ("MST" . -7)
    ("MDT" . -6)
    ("PST" . -8)
    ("PDT" . -7)))

(defun lookup-zone (zone)
  (let ((z (assoc zone +zone-table+ :test #'string-equal)))
    (if z
	(cdr z)
	0))) ; RFC2822 says interpret unknown symbolic zones as -0000

(defconstant +full-date-regexp+ 
  (compile-regexp #/"\([A-Za-z]+\), \([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\):\([0-9]+\):\([0-9]+\) \(+\|-\)\([0-9]+\)"))

(defconstant +day-of-week-missing-regexp+ 
  (compile-regexp #/"\([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\):\([0-9]+\):\([0-9]+\) \(+\|-\)\([0-9]+\)"))

(defconstant +seconds-missing-regexp+
  (compile-regexp #/"\([A-Za-z]+\), \([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\):\([0-9]+\) \(+\|-\)\([0-9]+\)"))

(defconstant +seconds-and-day-of-week-missing-regexp+
  (compile-regexp #/"\([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\):\([0-9]+\) \(+\|-\)\([0-9]+\)"))

(defconstant +full-date-with-obsolete-zone-regexp+ 
  (compile-regexp #/"\([A-Za-z]+\), \([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\):\([0-9]+\):\([0-9]+\) \([A-Za-z]+\)"))

(defconstant +day-of-week-missing-with-obsolete-zone-regexp+ 
  (compile-regexp #/"\([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\):\([0-9]+\):\([0-9]+\) \([A-Za-z]+\)"))

(defconstant +seconds-missing-with-obsolete-zone-regexp+
  (compile-regexp #/"\([A-Za-z]+\), \([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\) \([A-Za-z]+\)"))

(defconstant +seconds-and-day-of-week-missing-with-obsolete-zone-regexp+
  (compile-regexp #/"\([0-9]+\) \([A-Za-z]+\) \([0-9]+\) \([0-9]+\):\([0-9]+\) \([A-Za-z]+\)"))

(defparameter *date-format-table*
  `((:full-date . ,+full-date-regexp+)
    (:day-of-week-missing . ,+day-of-week-missing-regexp+)
    (:seconds-missing . ,+seconds-missing-regexp+)
    (:seconds-and-day-of-week-missing . ,+seconds-and-day-of-week-missing-regexp+)
    (:full-date-with-obsolete-zone . ,+full-date-with-obsolete-zone-regexp+)
    (:day-of-week-missing-with-obsolete-zone . ,+day-of-week-missing-with-obsolete-zone-regexp+)
    (:seconds-missing-with-obsolete-zone . ,+seconds-missing-with-obsolete-zone-regexp+)
    (:seconds-and-day-of-week-missing-with-obsolete-zone . ,+seconds-and-day-of-week-missing-with-obsolete-zone-regexp+)))

(defun regexp-for-date-format (date-format)
  (cdr (assoc date-format *date-format-table* :test #'string-equal)))

(defmethod parse-date-time ((dt date-time))
  (multiple-value-bind (not-comments comments)
      (remove-comments (date-time-original dt))
    (setf (date-time-comments dt) comments)
    (flet ((encode (date month year hour minute second time-zone)
	     (encode-universal-time (parse-integer second)
				    (parse-integer minute)
				    (parse-integer hour)
				    (parse-integer date)
				    (position month +month-table+ :test #'string-equal)
				    (parse-integer year)
				    time-zone)))
      (loop for date-format in (mapcar #'car *date-format-table*)
	    as match = (multiple-value-list (match-regexp (regexp-for-date-format date-format) not-comments :return :string))
	    do (if (eql (car match) t)
		   (case date-format
		     (:full-date
		      (destructuring-bind (success whole day-name date month year hour minute second plusminus zone) match
			(declare (ignore day-name success whole))
			(setf (date-time-ut dt) (encode date month year hour minute second (convert-rfc2822-zone plusminus zone)))))
		     (:day-of-week-missing
		      (destructuring-bind (success whole date month year hour minute second plusminus zone) match
			(declare (ignore success whole))
			(setf (date-time-ut dt) (encode date month year hour minute second (convert-rfc2822-zone plusminus zone)))))
		     (:seconds-missing
		      (destructuring-bind (success whole day-name date month year hour minute plusminus zone) match
			(declare (ignore day-name success whole))
			(setf (date-time-ut dt) (encode date month year hour minute "0" (convert-rfc2822-zone plusminus zone)))))
		     (:seconds-and-day-of-week-missing
		      (destructuring-bind (success whole date month year hour minute plusminus zone) match
			(declare (ignore success whole))
			(setf (date-time-ut dt) (encode date month year hour minute "0" (convert-rfc2822-zone plusminus zone)))))
		     (:full-date-with-obsolete-zone
		      (destructuring-bind (success whole day-name date month year hour minute second zone) match
			(declare (ignore day-name success whole))
			(setf (date-time-ut dt) (encode date month year hour minute second (lookup-zone zone)))))
		     (:day-of-week-missing-with-obsolete-zone
		      (destructuring-bind (success whole date month year hour minute second zone) match
			(declare (ignore success whole))
			(setf (date-time-ut dt) (encode date month year hour minute second (lookup-zone zone)))))
		     (:seconds-missing-with-obsolete-zone
		      (destructuring-bind (success whole day-name date month year hour minute zone) match
			(declare (ignore day-name success whole))
			(setf (date-time-ut dt) (encode date month year hour minute "0" (lookup-zone zone)))))
		     (:seconds-and-day-of-week-missing-with-obsolete-zone
		      (destructuring-bind (success whole date month year hour minute zone) match
			(declare (ignore success whole))
			(setf (date-time-ut dt) (encode date month year hour minute "0" (lookup-zone zone))))))))))
  dt)

; Tests
; Tue, 1 Jul 2003 10:52:37 +0200
; Fri, 21 Nov 1997 11:00:00 -0600
; Fri, 21 Nov 1997 11:00 -0600
; 21 Nov 97 09:55:06 GMT
; 21 Nov 97 09:55 GMT

;(time-string (date-time-ut (parse-date-time (make-instance 'date-time :original "Wed, 21 Sep 2005 16:03:22 -0500 (CDT)"))) :decdate)
