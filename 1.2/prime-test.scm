(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))

(define (divides? test-divisor n)
  (= 0 (remainder n test-divisor)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod a b n)
  (cond ((= b 0) 1)
		((even? b)
		 (remainder (square (expmod a
									(/ b 2)
									n)) n))
		(else
		  (remainder (* a (expmod a
								  (- b 1)
								  n)) n))))

