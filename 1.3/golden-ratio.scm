(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
	(< (abs (- x y)) tolerance))
  (let ((next (f first-guess)))
	(if (close-enough? first-guess next)
	  first-guess
	  (fixed-point f next))))

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1.0 x)))
				 1.0))
