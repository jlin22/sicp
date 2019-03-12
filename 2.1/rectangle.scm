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

(define (make-rect1 top-left bottom-right) (cons top-left bottom-right))

(define (top-left-rect1 r) (car r))
(define (bottom-right-rect1 r) (cdr r))

(define (height-rect1 r) (- (y-point (top-left-rect1 r))
							(y-point (bottom-right-rect1 r))))

(define (width-rect1 r) (- (x-point (bottom-right-rect1 r))
						   (x-point (top-left-rect1 r))))

(define (perim-rect1 r) (* 2 (+ (height-rect1 r) (width-rect1 r))))

(define (area-rect1 r) (* (height-rect1 r) (width-rect1 r)))

(define rect1 (make-rect1 (make-point 0 5)
						  (make-point 5 0)))

(define (make-dims height width) (cons height width))

(define (height-dim d) (car d))
(define (width-dim d) (cdr d))

(define (make-rect2 top-left height width)
  (cons top-left (make-dims height width)))

(define (top-left-rect2 r) (car r))
(define (dim-rect2 r) (cdr r))
(define (height-rect2 r) (height-dim (dim-rect2 r)))
(define (width-rect2 r) (width-dim (dim-rect2 r)))

(define (perimeter-rect2 r) (* 2 (+ (height-rect2 r) (width-rect2 r))))
(define (area-rect2 r) (* (height-rect2 r) (width-rect2 r)))


