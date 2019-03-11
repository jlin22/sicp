(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (display first-guess)
  (define (close-enough? x y)
	(< (abs (- x y)) tolerance))
  (let ((next (f first-guess)))
	(if (close-enough? first-guess next)
	  first-guess
	  (fixed-point f next))))

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
			   1.0))
