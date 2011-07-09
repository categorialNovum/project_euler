(load "euler.lisp")
(setf zero (cons 0 (expt 0 5)))
(setf one (cons 1 (expt 1 5)))
(setf two (cons 2 (expt 2 5)))
(setf three (cons 3 (expt 3 5)))
(setf four (cons 4 (expt 4 5)))
(setf five (cons 5 (expt 5 5)))
(setf six (cons 6 (expt 6 5)))
(setf seven (cons 7 (expt 7 5)))
(setf eight (cons 8 (expt 8 5)))
(setf nine (cons 9 (expt 9 5)))
(setf n5 (list zero one two three four five six seven eight nine))

(loop for a in n5 do
	(loop for b in n5 do
		(loop for c in n5 do
			(loop for d in n5 do
				(loop for e in n5 do
					(loop for f in n5 do
					(setf x (cons (car a) (cons (car b) (cons (car c) (cons (car d) (cons (car e) (cons (car f) nil)))))))
					(setf sum (+ (cdr a) (cdr b) (cdr c) (cdr d) (cdr e) (cdr f)))
					(if (equalp x (split-digits (write-to-string sum)))
						(format t "~&~s = ~s" x sum))
				))))))

(print "-------------------")

(loop for a in n5 do
	(loop for b in n5 do
		(loop for c in n5 do
			(loop for d in n5 do
				(loop for e in n5 do
					(setf x (cons (car a) (cons (car b) (cons (car c) (cons (car d) (cons (car e) nil))))))
					(setf sum (+ (cdr a) (cdr b) (cdr c) (cdr d) (cdr e)))
					(if (equalp x (split-digits (write-to-string sum)))
						(format t "~&~s = ~s" x sum))
				)))))

(print "-------------------")

(loop for a in n5 do
	(loop for b in n5 do
		(loop for c in n5 do
			(loop for d in n5 do
					(setf x (cons (car a) (cons (car b) (cons (car c) (cons (car d) nil)))))
					(setf sum (+ (cdr a) (cdr b) (cdr c) (cdr d)))
					(if (equalp x (split-digits (write-to-string sum)))
						(format t "~&~s = ~s" x sum))
				))))

