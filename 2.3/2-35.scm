(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))

(define (count-leaves tree)
  (display (map tree-leaves tree))
  (accumulate (lambda (x y) (+ y 1))
	      0
	      (tree-leaves tree)

(define (tree-leaves tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree))
	(else (append (tree-leaves (car tree))
		      (tree-leaves (cdr tree))))))
