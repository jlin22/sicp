(define (fold-left proc init sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (proc result (car rest))
	      (cdr rest))))
  (iter init sequence))

(define (fold-right proc init sequence)
  (if (null? sequence)
      init
      (proc (car sequence) (fold-right proc init (cdr sequence)))))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

(define (reverse2 sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
