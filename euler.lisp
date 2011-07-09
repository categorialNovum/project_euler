;(defun is-primep (x)
;	(if (< x 0)
;		(return-from is-primep nil))
;	(loop for i from 2 to (- x 1) do
;		(when (zerop (mod x i))
;			(return-from is-primep nil)))
;	t)

(defun is-primep (x)
	(if (< x 0)
		(return-from is-primep nil))
	(loop for i from 2 to (sqrt x) do
		;(print i)
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

;(defun make-prime-list (low limit)
;	(cond ((< limit 2) 
;		nil)
;	((= low limit) 
;		nil)
;	((is-primep low) 
;		(cons low (make-prime-list(+ low 1) limit)))
;	(t(make-prime-list (+ 1 low) limit))))
	
(defun fibonacci (a b)
	(setf c (+ a b))
	(cond ((>=  b 10000000000000000000000000)
			;(print a)
			;(print b)
			b)
	(t (print b) (fibonacci b c))))

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

(defun split-char (str)
	(loop for i from 0 to (1- (length str))
		collect (subseq str i (1+ i))
		))


(defun concat-ints (i j)
	(parse-integer (concatenate 'string (write-to-string i) (write-to-string j))))

(defun concat-int-list (l)
	(cond ((equalp 1 (length l))
		(write-to-string (first l)))
	(t(concatenate 'string (write-to-string (first l)) (concat-int-list (rest l))))))

(defun int-list-to-int (i)
	(parse-integer (concat-int-list i)))

(defun read-file (filename)
	(setf paths nil)
	(with-open-file (file filename)
		(loop until (null (setf line (read-next file)))do
			(setf paths (append paths (list line)))
		))
		paths)

(defun read-next (fin)
	(read-line fin nil))

(defun split-string (str)
	(loop for i = 0 then (1+ j)
		as j = (position #\Space str :start i)
		collect (subseq str i j)
		while j))

(defun convert-to-binary (n)
	(setf biggest 1)
	(setf exponent 0)
	(setf binTable nil)
	(setf bits nil)
	(loop while (<= biggest n) do
		(setf biggest (expt 2 exponent))
		(setf binTable (append binTable (list biggest)))
		(setf exponent (1+ exponent)))

	(loop for x in (reverse (butlast binTable)) do
		(cond ((>= n x) 
				(setf bits (append bits (list 1)))
			    (setf n (- n x)))	
			((< n x)
				(setf bits (append bits (list 0))))))
bits)

(defun memp (e l)
	(loop for x in l do
		(if (equal e x)
			(return-from memp t)))nil)

(defun permutations (lst)
  (if (null lst) '(())
  (mapcan #'(lambda (x)
	      (mapcar #'(lambda (y) (cons x y))
		      (permutations (remove x lst :count 1))))
	  lst)))

(defun triangle-num (n)
  (/ (* n (1+ n)) 2))

(defun pentagonal-num (n)
  (/ (*  n (1- (* 3 n))) 2))

(defun hexagonal-num (n)
  (* n (1- (* 2 n))))
