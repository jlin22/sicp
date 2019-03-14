; Deep reverse: Input: tree, Output: tree with sublists reversed

(define (deep-reverse l)
  (cond ((null? l) ())
		((not (pair? l)) l)
		(else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define l (list 1 2 3 4))
(define x (list (list 2 3) (list 4 5)))
  

