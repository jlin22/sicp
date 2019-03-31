(define (unique-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
	    (map (lambda (k)
		   (list i j k))
		 (enumerate-interval 1 (- j 1))))
	  (enumerate-interval 1 (- i 1)))
     (enumerate-interval 1 n))))
  
(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq) (accumulate proc init (cdr seq)))))

(define (flatmap proc seq) (accumulate append () (map proc seq)))

(define (enumerate-interval s e)
  (if (> s e)
      ()
      (cons s (enumerate-interval (+ s 1) e))))
