(define (expt a b)
  (if (= b 0)
	1
	(* a (expt a (- b 1)))))

(define (expt-iter a b)
  (define (expt-helper result counter)
	(if (= counter b)
	  result
	  (expt-helper (* result a) (+ counter 1)))
	)
  (expt-helper 1 0))

(define (fast-expt a b)
  (cond ((= b 0) 1)
		((even? b) (square (fast-expt a (/ b 2))))
		(else (* a (fast-expt a (- b 1))))))

; Exercise 1.16
; Create an iterative version of fast-expt

(define (fast-expt-iter b n)
  (define (fast-expt-iter a b n)
	(if (= n 1)
	  (* a b)
	  (if (even? n)
		(fast-expt-iter a (square b) (/ n 2))
		(fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))
