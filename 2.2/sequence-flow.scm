(define (filter predicate sequence)
  (cond ((null? sequence) ())
		((predicate (car sequence))
		 (cons (car sequence) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (cond ((null? sequence) initial)
		(else (op (car sequence)
				  (accumulate op initial (cdr sequence))))))

(define (enumerate-interval low high)
  (if (> low high)
	()
	(cons low (enumerate-interval (+ low 1) high))))
		
(define (enumerate-leaves tree)
  (cond ((null? tree) ())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-leaves (car tree))
					  (enumerate-leaves (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
			  0
			  (map square
				   (filter odd?
						   (enumerate-leaves tree)))))

(define (even-fibs n)
  (accumulate cons
			  ()
			  (filter even?
					  (map fib
						   (enumerate-interval 0 n)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
		((even? count)
		 (fib-iter 
		   a
		   b
		   (sum-of-squares p q)
		   (+ (square q) (* 2 p q))
		   (/ count 2)))
		(else
		  (fib-iter 
			(+ (* b q) (* a p) (* a q))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

(define (sum-of-squares p q) (+ (square p) (square q)))
