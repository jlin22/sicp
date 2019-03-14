(define (same-parity first . subsequent)
  (define (helper x l)
	(cond ((null? l) ())
		  ((= 0 (remainder (- (car l) x) 2)) 
		   (append (list (car l)) (helper x (cdr l))))
		  (else
			(helper x (cdr l)))))
  (append (list first) (helper first subsequent)))
