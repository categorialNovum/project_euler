(defun splitDigits (n)
	(setq num nil)
	(loop for i from 0 to (1- (length n)) 
		collect (parse-integer (subseq n i (1+ i)))))

(defun splitAll (n)
	(cond ((zerop (length n))
			nil)
		(t (cons (reverse (splitDigits (first n))) (splitAll (rest n))))))	


(defun addTwoBig (a b total)
	(cond ((zerop (length a))
			total)
		(t (addTwoBig (rest a) (rest b) (append total (list (+ (first a) (first b))))))))
			

(defun performCarry (l)
	(setf carry 0)
	(setf sum nil)
	(loop for x in l do
		(setf i (+ carry x))
		;(format t "~&----------------------------------")
		;(format t "~&~s" i)
		;(format t "~&m : ~s" (mod i 10))
		(setf sum (append sum (list (mod i 10))))
		;(format t "~&~s" sum)
		(cond ((< i 10)
				(setf carry 0))
			((and (> i 9) (< i 20))
				(setf carry 1))
		)
	)
	(setf sum (append sum (list carry)))
	sum)


(defun add (l)
	(setf nums nil)
	(cond ((eq (length l) 1)
			(return-from add l))
		((evenp (length l))
			(format t "~& even : ~s" (length l))
			(loop for x from 0 to (1- (length l)) by 2 do
				;(format t "~&~s" x)
				(setf nums (append nums (list(performCarry (addTwoBig (nth x l) (nth (1+ x)l) nil)))))))
		((oddp (length l))
			(format t "~& odd : ~s"  (length l))
			(loop for x from 0 to (- (length l) 2) by 2 do
				(format t "~&~s" x)
				(setf nums (append nums (list(performCarry (addTwoBig (nth x l) (nth (1+ x)l) nil))))))
				
			(setf nums (append nums (list (append (car (last l)) (list 0)))))
			))
		(add (remove nil nums)))
							
	

