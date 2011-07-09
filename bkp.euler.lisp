(defun is-primep (x)
	(if (< x 0)
		(return-from is-primep nil))
	(loop for i from 2 to (- x 1) do
		(when (zerop (mod x i))
			(return-from is-primep nil)))
	t)

(defun largest-prime-factor (start num)
	(cond ((zerop start)
		nil)
	((= start num)
		(largest-prime-factor (- start 1) num))
	((zerop (mod num start))
		start)
	(t(largest-prime-factor (nextLowestPrime (- start 1)) num))))
	

(defun nextPrime (x)
	(if (is-primep x) (return-from nextPrime x))
	(nextPrime (+ x 1))
	)

(defun nextLowestPrime (x)
	(if (is-primep x) (return-from nextLowestPrime x))
	(nextLowestPrime (- x 1))
	)

(defun largestPrimeFactor (x)
	(loop for i from 2 to x do
		(cond ((zerop (mod x i))
				(if (is-primep (/ x i))
					(return-from largestPrimeFactor (/ x i)))))))

(defun make-prime-list (low limit)
	(cond ((< limit 2) 
		nil)
	((= low limit) 
		nil)
	((is-primep low) 
		(cons low (make-prime-list(+ low 1) limit)))
	(t(make-prime-list (+ 1 low) limit))))
	
(defun fibonacci (a b)
	(setf c (+ a b))
	(cond ((> (length c) 10)
			c)
	(t (print (fibonacci b c)))))
	

(defun testFor (a b c)
	(loop for x from a to b by c do
		(print x)))

(defun n-bang (n)
	(if (eq n 1)
		1
	(* n (n-bang (1- n)))))

(defun addList (l)
	(if (zerop (length l)) 
		0	
	(+ (first l) (addList (rest l)))))

(defun split-digits (n)
	(setq num nil)
	(loop for i from 0 to (1- (length n)) 
		collect (parse-integer (subseq n i (1+ i)))))

