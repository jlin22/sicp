(define (make-segment p q) (cons p q))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s) 
  (let ((start-x (x-point (start-segment s)))
		(start-y (y-point (start-segment s)))
		(end-x (x-point (end-segment s)))
		(end-y (y-point (end-segment s))))
	(make-point (average start-x end-x)
				(average start-y end-y))))

(define (average x y) (/ (+ x y) 2))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (newline)
  (display "start")
  (print-point (start-segment s))
  (newline)
  (display "end")
  (print-point (end-segment s)))

(define s (make-segment (make-point 1 2) (make-point 3 4)))
