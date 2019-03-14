(define (fringe lst)
  (cond ((null? lst) (display ""))
		((not (pair? lst)) (display lst) (display " "))
		(else (fringe (car lst))
			  (fringe (cdr lst)))))
	
