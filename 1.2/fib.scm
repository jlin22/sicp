(define (fib-rec n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib-rec (- n 1)) (fib-rec (- n 2))))))

(define (fib-iter n)
  (define (fib-helper a b count)
	; count = n, b = (fib 0)
	(if (= count 0)
	  b
	  (fib-helper (+ a b) a (- count 1))))
  (fib-helper 1 0 n))
