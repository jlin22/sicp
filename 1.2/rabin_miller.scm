(define (prime? n times)
  (cond ((= times 0) #t)
		((rabin-miller-test n) (prime? n (- times 1)))
		(else #f)))

(define (rabin-miller-test n)
  (define (test-it a)
	(= (expmod a (- n 1) n) 0))
  (test-it (+ 1 (random (- n 1)))))

(define (expmod a b n)
  (cond ((= b 0) 1)
		((even? b)
		 (check-nontrivial-sqrt (remainder (square (expmod a
												   (/ b 2)
												   n)) n) n))
		(else
		  (remainder (* a (expmod a (- b 1) n)) n))))

(define (check-nontrivial-sqrt a n)
  (let ((x (remainder (square a) n)))
	(if (and (not (= a 1)) (not (= a (- n 1))) (= x 1))
	  0
	  a
	  )))
