(define (list-ref items n)
  (if (= n 0)
	(car items)
	(list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
	0
	(+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (length-helper items result)
	(if (null? items)
	  result
	  (length-helper (cdr items) (+ 1 result))))
  (length-helper items 0))

(define (append items1 items2)
  (if (null? items1)
	items2
	(cons (car items1) (append (cdr items1) items2))))

