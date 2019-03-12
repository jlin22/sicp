(define (nth-root x n)
  (define (damps n)
	(cond ((or (= n 2) (= n 3)) average-damping)))
  (fixed-point (damps (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(define (fixed-point f first-guess)
  (define (close-enough? 
