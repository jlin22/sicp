(define (cube-root-iter guess x)
  (if (good-enough? guess x)
	guess
	(cube-root-iter (improve guess x) x)))

(define epsilon 10e-12)
(define (good-enough? guess x)
  (< (abs (- (improve guess x) guess)) (* epsilon guess)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube x) (* x x x))

(define (square x) (* x x))

(define (cube-root x) (cube-root-iter 1.0 x))
