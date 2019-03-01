(define (fast-prime? n times)
  (cond ((= times 0) #t)
		((fermat-test n times) (fast-prime? n (- times 1)))
		(else #f)))

(define (fermat-test n b)
  (= (expmod b n n) b))

(define (expmod a b n)
  (cond ((= b 0) 1)
		((even? b) (remainder (square (expmod a
									  (/ b 2)
									  n)) n))
		(else (remainder (* a (expmod a
									  (- b 1)
									  n)) n))))
