(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point)
	  midpoint
	  (let ((test-value (f midpoint)))
		(cond ((positive? test-value)
			   (search f neg-point test-value))
			  ((negative? test-value)
			   (search f test-value pos-point))
			  (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.01))

(define (average x y) (/ (+ x y) 2.0))

(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (positive? b-value) (negative? a-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else (error "Values are not of opposite signs" a b)))))
