(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (includes-zero? y)
	(error "divisor spans 0")
	(mul-interval x
				  (make-interval (/ 1.0 (upper-bound y))
								 (/ 1.0 (lower-bound y))))))
(define (includes-zero? x)
  (and (< (lower-bound x) 0) (> (upper-bound x) 0)))

(define (make-interval a b)
  (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
				 (- (upper-bound x) (lower-bound y))))
				
(define (sub-interval x y)
  (add-interval x
				(make-interval (- 0 (upper-bound y))
							   (- 0 (lower-bound y)))))

(define (mul-interval x y)
  (let ((sgn-x-low (sgn (lower-bound x)))
		(sgn-x-up (sgn (upper-bound x)))
		(sgn-y-low (sgn (lower-bound y)))
		(sgn-y-up (sgn (upper-bound y))))
	(cond ((and (plus? sgn-x-low) (plus? sgn-x-up)
				(plus? sgn-y-low) (plus? sgn-y-up))
		   (make-interval (* (lower-bound x) (lower-bound y))
						  (* (upper-bound x) (upper-bound y))))
		  ((and (plus? sgn-x-low) (plus? sgn-x-up)
				(minus? sgn-y-low) (minus? sgn-y-up))
		   (make-interval (* (upper-bound x) (lower-bound y))
						  (* (lower-bound x) (upper-bound y))))
		  ((and (plus? sgn-x-low) (plus? sgn-x-up)
				(minus? sgn-y-low) (plus? sgn-y-up))
		   (make-interval (* (upper-bound x) (lower-bound y))
						  (* (upper-bound x) (upper-bound x))))
		  ((and (minus? sgn-x-low) (plus? sgn-x-up)
				(minus? sgn-y-low) (plus? sgn-x-up))
		   (let ((p1 (* (lower-bound x) (lower-bound y)))
				(p2 (* (lower-bound x) (upper-bound y)))
				(p3 (* (upper-bound x) (lower-bound y)))
				(p4 (* (upper-bound x) (upper-bound y))))
			(make-interval (min p2 p3)
						   (max p1 p4))))
		  ((and (minus? sgn-x-low) (plus? sgn-x-up)
				(plus? sgn-y-low) (plus? sgn-y-up))
		   (make-interval (* (upper-bound y) (lower-bound x))
						  (* (upper-bound y) (upper-bound x))))
		  ((and (minus? sgn-x-low) (minus? sgn-x-up)
				(minus? sgn-y-low) (minus? sgn-y-up))
		   (make-interval (* (upper-bound x) (upper-bound y))
						  (* (lower-bound x) (lower-bound y))))
		  (else (mul-interval y x)))))

(define (sgn x)
  (if (< x 0)
	-1
	+1))

(define (plus? s) (> s 0))
(define (minus? s) (< s 0))
; this last line makes it so you don't have to have 9 cases, only 6,
; but it's slower because it has redundant checks
; if you wanted to implement this 9 cases, do
; - +, + +; - -, + +; and - -, - +

(define (make-center-percent center percent)
  (make-interval (* center (/ (- 1.0 percent) 100.0))
		 (* center (/ (+ 1.0 percent) 100.0))))
(define (center interval) (average (lower-bound interval) (upper-bound interval)))
(define (average x y) (/ (+ x y) 2))
(define (percent interval) (* 100 (/ (upper-bound interval) center)))
