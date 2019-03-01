(define (sum term a next b)
  (if (> a b)
	0
	(+ (term a) (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (sum-ints a b)
  (define (identity x) x)
  (sum identity a inc b))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (cube x) (* x x x))

(define (pi-sum a b)
  (define (pi-term x)
	(/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
	(+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx a) (+ a dx))
  (define (term a) (f a))
  (* dx (sum term (+ a (/ dx 2.0)) add-dx b)))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k) 
	(cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
		  ((even? k) (* 2 (f (+ a (* k h)))))
		  (else (* 4 (f (+ a (* k h)))))))
  (* (/ h 3) (sum term 0 inc n)))
