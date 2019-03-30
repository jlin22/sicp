(define (enumerate-interval s e)
  (if (> e s)
      ()
      (cons s (enumerate-interval (+ s 1) e))))

(define (pairs n)
  (accumulate append
	      ()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

(define (accumulate p init seq)
  (if (null? seq)
      init
      (p (car seq) (accumulate p init (cdr seq)))))
