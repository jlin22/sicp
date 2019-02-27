; (define (good-enough? guess x) (< (abs (- (square guess) x)) 0.0001))
; (define (sqrt x) (sqrt-iter 1.0 x))
; (define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;	guess
;	(sqrt-iter (improve guess x) x)))

(define epsilon 10e-12)
(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) (* guess epsilon)))
(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (if (good-enough guess x)
	guess
	(sqrt-iter (improve guess x) x)))

; (define (good-enough? guess x) (< (abs (- (square guess) x)) 0.0001))
; (square (sqrt 0.00000000001))
; good-enough is bad for small numbers, because for numbers less than 0.0001,
; we will return guess, when guess is far from our number. 
; (square (sqrt 9999999999999999))
; good-enough is bad for large numbers, because we will be unable to use many decimal places,
; so we might be off when we square the number,
; because we lost precision

; does this new sqrt-iter work better for small or large numbers?
; it works better for large numbers. small numbers can get too small for the constant to work
; for large numbers, this is good
