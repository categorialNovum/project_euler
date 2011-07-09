(load "euler.lisp")

;list follows the form (1^1 2^2 ..... n^n)
(defun mklist (n)
	(setf l nil)
	(loop for x from 1 to n do
		(setf l (append l (list (expt x x)))))
	l)
