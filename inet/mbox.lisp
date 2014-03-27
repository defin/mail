(in-package mail-inet)

(defun read-mbox-message (stream)
  (loop named msg
	for line = (read-line stream nil :eof)
	with lines = nil
	with newline = (string #\Newline)
	until (or (eq line :eof)
		  (and (> (length line) 5)
		       (search "From " line :start2 0 :end2 5 :test #'char=)
		       (print line)))
	do (push line lines) ; wrong.. in the body of a message, turn every >From at the beginning of a line into From
	   (push newline lines)
	finally (return-from msg (cond ((eq line :eof)
					(values lines :eof))
				       ((null lines)
					(values nil nil))
				       (t (values (append-string-list (nreverse (rest lines))) nil))))))

