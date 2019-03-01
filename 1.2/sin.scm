(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sin angle)
  (if (not (> (abs angle) 0.1))
	angle
	(sin (p (/ angle 3.0)))))

