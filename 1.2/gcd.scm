(define (gcd a b)
  (if (= b 0)
	a
	(gcd b (remainder a b))))

; Exercise 1.20
; Applicative Order
; (gcd 206 40)
; (gcd 40 6)
; (gcd 6 4)
; (gcd 4 2)
; (gcd 2 0)
; 2

; Normal Order
; (gcd 206 40)
; (gcd 40 (rem 206 40))
; (gcd (rem 206 40) (rem 40 (rem 206 40)))
; (gcd (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))
; Normal Order never terminates. It never reaches the base case, because
; It cannot tell what term becomes 0
