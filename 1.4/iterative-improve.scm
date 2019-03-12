(define (iterative-improve good-enough? improve-guess)
  (define (refine-guess guess)
	(if (good-enough? guess)
	  guess
	  (refine-guess (improve-guess guess))))
  refine-guess)

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
	(< (abs (- (square guess) x)) 0.0001))
  (define (improve-guess guess)
	(average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))

(define (fixed-point f first-guess)
  (define (good-enough? guess)
	(let ((next (f first-guess)))
	(< (abs (- guess next)) 0.0001)))
  ((iterative-improve good-enough? f) first-guess))
