(define (reverse l)
  (if (null? l)
	()
	(append (reverse (cdr l)) (list (car l)))))

(define (append list1 list2)
  (if (null? list1)
	list2
	(cons (car list1) (append (cdr list1) list2))))
