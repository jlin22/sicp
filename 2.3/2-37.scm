(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq) (accumulate proc init (cdr seq)))))

(define (dot-product u v)
  (accumulate + 0 (map * u v)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init
			(map (lambda (x) (car x)) seqs))
	    (accumulate-n op init
			  (map (lambda (x) (cdr x)) seqs)))))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector n row)) m)))
