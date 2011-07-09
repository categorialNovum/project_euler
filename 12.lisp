(load "euler.lisp")

;takes a parameter n, produces the nth triangle number
(defun triangle (n)
        (if (eq n 1)
                1
        (+ n (triangle (1- n)))))

(defun nDivisors (x)
        (setf d 0)
        (loop for i from 1 to x do
                (cond ((zerop (mod x i))
                        (setf d (1+ d)))))d)

(defun num-w-n-divisors (n)
        (setf done 'f)
        (setf nthTriangle 1)
        (loop while (eq done 'f) do
                (if (>= (nDivisors (triangle nthTriangle)) n)
                        (return-from num-w-n-divisors (triangle nthTriangle)))
                (setf nthTriangle (1+ nthTriangle))))
