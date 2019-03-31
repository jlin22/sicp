(define (unique-triples n)
  (flatmap
   (lambda (p)
     (map (lambda (k)
	    (append p (list k)))
	  (enumerate-interval 1 (- (cadr p) 1))))
   (unique-pairs n)))
  
(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq) (accumulate proc init (cdr seq)))))

(define (flatmap proc seq) (accumulate append () (map proc seq)))

(define (enumerate-interval s e)
  (if (> s e)
      ()
      (cons s (enumerate-interval (+ s 1) e))))

(define (unique-pairs n)
  (accumulate append
	      ()
	      (map (lambda (i)
		     (map (lambda (j)
			    (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))
