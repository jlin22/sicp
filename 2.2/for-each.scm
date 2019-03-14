(define (for-each proc lst)
  (define (apply-and-iter)
    (proc (car lst))
    (for-each proc (cdr lst)))
  (if (null? lst)
      ()
      (apply-and-iter)))


      
