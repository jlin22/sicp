(define (cont-frac n d k)
  (define (helper counter)
	(if (> counter k)
	  0
	  (/ (n counter) 
		 (+ (d counter) (helper (+ counter 1))))))
  (helper 1))

(define (cont-frac-iter n d k)
  (define (iter-helper result counter)
	(if (= counter 0)
	  result
	  (iter-helper (/ (n counter) (+ (d counter) result)) (- counter 1))))
  (iter-helper 0 k))

(define (phi-approx k)
  (define (d i) 1.0)
  (define (n i) 1.0)
  (/ 1 (cont-frac n d k)))

(define (check-precision actual approx k)
  (define (display-actual)
	(newline)
	(display "actual : ")
	(display actual))
  (define (helper counter)
	(let ((val (approx counter)))
	  (newline)
	  (display counter)
	  (display " : ")
	  (display val)
	  (if (= k counter)
		(display-actual)
		(helper (+ counter 1)))))
  (helper 1))

(define (e-approx k)
  (define (d i)
	(if (= (remainder (- i 2) 3) 0)
	  (* 2 (+ (/ (- i 2) 3) 1))
	  1))
  (define (n i) 1.0)
  (+ (cont-frac n d k) 2))

(define (tan-cf x k)
  (define (n i) (- 0 (expt x i)))
  (define (d i) (- (* 2.0 i) 1.0))
  (- 0 (cont-frac n d k)))

