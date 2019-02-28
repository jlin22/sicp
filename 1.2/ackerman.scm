(define (A x y)
  (cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1)
				 (A x (- y 1))))))

; (A 1 10) -> 1024
; (A 2 4) -> 2 ** 16
; (A 3 3) -> 2 ** 16

(define (f n) (A 0 n))
; 2 * n

(define (g n) (A 1 n))
; 2 ** n if n > 0, 0 if n == 0

(define (h n) (A 2 n))
; 2 ** 2 ** 2 ** ... ** 2, with power occuring n - 1 times

(define (recursive-2-to-power n)
  (cond ((= n 0) 0)
		((= n 1) 2)
		(else (expt 2 (recursive-2-to-power (- n 1))))))

(define (h-iter n)
  (define (h-iter-helper counter result n)
	(if (> counter n)
	  result
	  (h-iter-helper
		(+ counter 1)
		(expt 2 result)
		n)))
  (if (= n 0)
	0
	(h-iter-helper 1 1 n)))

