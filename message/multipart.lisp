(in-package mime)

#.(sharpslash:install)

(defun split-body-by-boundary (body boundary)
  (let ((matched-final-boundary-p nil)
	(dash-boundary `(:sequence
			 (:register (:sequence (:greedy-repetition 0 1 #\Newline)
					       "--" ,boundary))                         	; #/"(\n?--"
			 (:non-greedy-repetition 0 1 (:register "--"))
			 (:register (:alternation :end-anchor
						  (:sequence (:greedy-repetition 0 nil :whitespace-char-class)
							     :end-anchor))))))                  ; #/")(--)??($|\s*$)"
    (flet ((re/search (start)
	     (multiple-value-bind (success whole boundary-mark final-dashes)
		 (match-re dash-boundary body :return :index :start start :multiple-lines t)
	       (declare (ignore success whole))
	       (when final-dashes
		 (setf matched-final-boundary-p t))
	       boundary-mark)))
      (loop with body-part-start = 0
	    with body-part-end = 0
	    for end-of-boundary = (cdr (re/search body-part-start))
	    do (setf body-part-start end-of-boundary
		     body-part-end (car (re/search body-part-start)))
	    collect (subseq body (1+ body-part-start) body-part-end) into body-parts
	    when matched-final-boundary-p
	      return body-parts))))

