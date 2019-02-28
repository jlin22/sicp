(define (factorial-recursive n)
  (if (= n 1)
	1
	(* n (factorial-recursive (- n 1)))))

(define (factorial-iterative n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
	product
	(fact-iter (* product counter)
			   (+ counter 1)
			   max-count)))
