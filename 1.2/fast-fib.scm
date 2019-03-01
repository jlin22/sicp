; Exercise 1.19
; Fast fibonacci

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
		((even? count)
		 (fib-iter 
		   a
		   b
		   (sum-of-squares p q)
		   (+ (square q) (* 2 p q))
		   (/ count 2)))
		(else
		  (fib-iter 
			(+ (* b q) (* a p) (* a q))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

(define (sum-of-squares p q) (+ (square p) (square q)))
