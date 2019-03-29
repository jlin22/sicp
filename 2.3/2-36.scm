(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init
			(map (lambda (x) (car x)) seqs))
	    (accumulate-n op init
			(map (lambda (x) (cdr x)) seqs)))))
