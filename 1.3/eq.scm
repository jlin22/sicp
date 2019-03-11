(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (fixed-point-print f first-guess counter)
	  (newline)
	  (display counter)
	  (display " : ")
	  (display first-guess)
	  (define (close-enough? x y)
		(< (abs (- x y)) tolerance))
	  (let ((next (f first-guess)))
		(if (close-enough? first-guess next)
		  first-guess
		  (fixed-point-print f next (+ counter 1)))))
  (fixed-point-print f first-guess 1))

(define (average x y) (/ (+ x y) 2))

(define (no-damping)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
			   2.0))
  
(define (with-damping)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
			   2.0))



