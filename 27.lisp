(load "euler.lisp")

(defun eval-quadratic (n a b)
	;(format t "~&~s" (+ (expt n 2) (* a n) b))
	(+ (expt n 2) (* a n) b))

(defun consecutive-primes (a b)
	(setf n 0)
	(setf done 'f)
	(loop  while (eq 'f done) do
		;(format t "~&~s" (eval-quadratic n a b))
		(if (not (is-primep (eval-quadratic n a b)))
			(return-from consecutive-primes n))
		(setf n (1+ n))))

(defun iterate ()
	(setf best 0)
	(setf a 0)
	(setf b 0)
	(loop for x from -1000 to 1000 do
		(loop for y from -1000 to 1000 do
			(setf new (consecutive-primes x y))
			(cond ((> new best)
					(setf best new)
					(setf a x)
					(setf b y)
					(format t "~&new best : ~s" best)
					(format t "~&a : ~s" a)
					(format t "~&b : ~s" b)
					(format t "~&----------------------------" )))))					
	(format t "~&************************")	
	(format t "~&a : ~s" a)	
	(format t "~&b : ~s" b)				
	(format t "~&n : ~s" best)
)
					
