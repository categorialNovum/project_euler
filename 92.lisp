(load "euler.lisp")


(defun follow-chain (num)
	(cond ((eq 1 num)  
			(return-from follow-chain 0))
		((eq 89 num) (return-from follow-chain 1))
		(t 
			(follow-chain (root-add(split-digits (write-to-string num)))))
	))


(defun root-add (num)
	(cond ((zerop (length num))
			0)
		(t (+ (expt (car num) 2) (root-add (rest num))))))
		

(setf ct 0)
(loop for x from 2 to 10000000 do
	(setf ct (+ ct (follow-chain x)))
	)
(format t "~& Final Count : ~s" ct)

