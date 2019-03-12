(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (cons x y)
  (define (dispatch m)
	(cond ((= m 0) x)
		  ((= m 1) y)
		  (else ())))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (if (= 0 (remainder z 2))
	(+ 1 (car (/ z 2)))
	0))

(define (cdr z)
  (if (= 0 (remainder z 3))
	(+ 1 (car (/ z 3)))
	0))

