(in-package mail-process)

(defparameter *substitution-begin-sequence* "[[")
(defparameter *substitution-end-sequence* "]]")

(defun find-next-substitution (string)
  (let* ((start-outside (search *substitution-begin-sequence* string))
	 (start-inside (and start-outside (+ start-outside (length *substitution-begin-sequence*))))
	 (end-inside (and start-inside (search *substitution-end-sequence* string :start2 start-inside)))
	 (end-outside (and end-inside (+ end-inside (length *substitution-end-sequence*)))))
    (when end-outside
      (list (intern (string-upcase (subseq string start-inside end-inside)) 'keyword)
	    start-outside
	    end-outside))))

(defun replace-substitution (string sub)
  (destructuring-bind (substitution start end) sub
    (let ((substitute (expand-substitution substitution)))
      (concatenate 'string (subseq string 0 start) substitute (subseq string end)))))

(defun do-substitutions (string)
  (let ((s string))
    (loop for sub = (find-next-substitution s)
	  while sub
	  do (setf s (replace-substitution s sub)))
    s))

(defmethod expand-substitution ((sub t))
  (error "Don't know how to expand substitution ~S." sub))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(expand-substitution do-subtitutions replace-substition find-next-substitution)))



