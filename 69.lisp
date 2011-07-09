(load "euler.lisp")

(defun gen-x-primes (primes x)
	(cond ((eq (length primes) x)
			(return-from gen-x-primes primes))
		(t (push (nextPrime (1+ (first primes))) primes)
			(gen-x-primes primes  x)
		)
))	

(defun gen-primes-to (primes x)
	(cond ((>= (first primes) x)
			(return-from gen-primes-to primes))
		(t (push (nextPrime (1+ (first primes))) primes)
			(gen-primes-to primes  x)
		)
))	


(setf p (list 2))
(setf plist (gen-primes-to p 100000))

(print (length plist))
;(print plist)
	


