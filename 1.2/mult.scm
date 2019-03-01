; Exercise 1.17
; Given fns, double and halve, come up with a logarithmic multiplication alg

(define (* a b)
  (cond ((= b 1) a)
		((even? b) (* (double a) (halve b)))
		(else (+ (* a (- b 1)) a))))

(define (fast-mult a b)
  (define (mult-helper extra a b)
	(cond ((= b 1) (+ extra a))
		  ((even? b) (mult-helper extra (double a) (halve b)))
		  (else (mult-helper (+ extra a) a (- b 1)))))
  (mult-helper 0 a b))

(define (double x) (+ x x))

(define (halve x) (/ x 2))
