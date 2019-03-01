(define (val-of-change n)
	(cond ((= n 1) 1)
	  ((= n 2) 5)
	  ((= n 3) 10)
	  ((= n 4) 25)))

(define (cc-helper amount kinds-of-change)
	(cond ((= amount 0) 1)
		  ((or (< amount 0) (= kinds-of-change 0)) 0)
		  (else
			(+ (cc-helper 
				 (- amount (val-of-change kinds-of-change))
				 kinds-of-change)
			   (cc-helper
				 amount
				 (- kinds-of-change 1))))))

(define (count-change amount)
  (cc-helper amount 4))

; space and run time complexity?
; run time is theta(n ** 5)
; t(x, k) is asymptotically x / n * t(x, k-1)
; space is theta(n), deferred operations = height of recursion tree
; = n
