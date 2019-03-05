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
