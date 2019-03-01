(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 20)
	(report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (timed-prime-test start)
  (if (not (< start end))
	(newline)
	(search-for-primes (+ start 1) end)))
	 
(define (prime? n)
  (= n (smallest-divisor n 2)))

(define (smallest-divisor n count)
  (cond ((> (square count) n) n)
		((divides? count n) count)
		(else (smallest-divisor n (next count)))))

(define (next n)
  (if (= n 2)
	3
	(+ n 2)))

(define (divides? count n)
  (= (remainder n count) 0))

; Exercise 1.22, don't use fast prime

(define (fast-prime? n times)
  (cond ((= times 0) #t)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))

(define (fermat-test n)
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (floor (random (- n 1))))))

(define (expmod a p n)
  (cond ((= p 0) 1)
		((even? p) (remainder (square (expmod a
											  (/ p 2)
											  n)) n))
		(else (remainder (* a (expmod a
									  (- p 1)
									  n)) n))))
