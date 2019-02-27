(define (average x y) (/ (+ x y) 2))
(define epsilon 10e-6)

(define (sqrt x)
  (define (improve guess)
	(average guess (/ x guess)))
  (define (good-enough? guess)
	#t)
;	(< (abs (- (improve guess) guess)) (* guess epsilon)))
  (define (sqrt-iter guess)
	(if (good-enough? guess)
	  guess
	  (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
