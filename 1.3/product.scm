(define (product term a next b)
  (if (> a b)
	1
	(* (term a) (product term (next a) next b))))

(define (identity n) n)
(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))
  
(define (pi-approx n)
  (* 4 (quarter-pi-approx n)))

(define (quarter-pi-approx n)
  (define (numerator n)
	(product num-term 1.0 inc n))
  (define (denominator n)
	(product denom-term 1.0 inc n))
  (define (num-term x)
	(if (even? x)
	  (+ x 2)
	  (+ x 1)))
  (define (denom-term x)
	(if (even? x)
	  (+ x 1)
	  (+ x 2)))
  (/ (numerator n) (denominator n)))

(define (prod-iter term a next b)
  (define (iter a result)
	(if (> a b)
	  result
	  (iter (next a) (* (term a) result))))
  (iter a 1))
