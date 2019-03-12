(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define dx 0.0001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
				  1.0))

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
	(< (abs (- x y)) tolerance))
  (let ((next (f first-guess)))
	(if (close-enough? next first-guess)
	  first-guess
	  (fixed-point f next))))

(define (zeros-cubic a b c)
  (newtons-method (cubic a b c) 1))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (cube x) (* x x x))
