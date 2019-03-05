(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
		((filter (term a)) (combiner (term a)
									 (filtered-accumulate filter
														  combiner
														  null-value
														  term
														  (next a)
														  next
														  b)))
		(else (filtered-accumulate filter
								   combiner
								   null-value
								   term
								   (next a)
								   next
								   b))))

(define (sum-prime a b)
  (filtered-accumulate prime? + 0 identity a inc b))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (prime? x)
  (= (find-smallest-prime-factor x) x))

(define (find-smallest-prime-factor x)
  (define (helper y)
	(cond ((> (square y) x) x)
		  ((divides? y x) y)
		  (else (helper (+ y 1)))))
  (helper 2))

(define (divides? y x)
  (= (remainder x y) 0))

(define (gcd x y)
  (if (= y 0)
	x
	(gcd y (remainder x y))))

(define (sum-rel-prime n)
  (filtered-accumulate (rel-prime? n) * 1 identity 1 inc (- n 1)))

(define (rel-prime? n)
  (lambda (i) (= (gcd n i) 1)))
