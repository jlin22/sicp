(define (same-parity first . subsequent)
  (define (helper x l)
	(cond ((null? l) ()) 
		  ((both-same-parity? x (car l))
		   (append (list (car l)) (helper x (cdr l))))
		  (else
			(helper x (cdr l)))))
  (cons first (helper first subsequent)))

(define (both-same-parity? x y)
  (= 0 (remainder (- x y) 2)))
