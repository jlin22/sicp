#|
(define (for-each proc lst)
  (define (apply-and-iter)
    (proc (car lst))
    (for-each proc (cdr lst)))
  (if (null? lst)
      ()
      (apply-and-iter)))
|#

(define (for-each proc lst)
  (cond ((not (null? lst))
	 (proc (car lst))
	 (for-each proc (cdr lst)))))

      
