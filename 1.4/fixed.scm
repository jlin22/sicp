(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
	(< (abs (- x y)) tolerance))
  (let ((next (f first-guess)))
	(if (close-enough? next first-guess)
	  first-guess
	  (fixed-point f next))))

(define (average x y) (/ (+ x y) 2))

(define (average-damping f)
  (lambda (x) (average x (f x))))

(define (sqrt y)
  (fixed-point (average-damping (lambda (x) (/ y x))) 1.0))

(define (cube-root y)
  (fixed-point (average-damping (lambda (x) (/ y (square x)))) 1.0))
