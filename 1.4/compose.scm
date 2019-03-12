(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
	(lambda (x) x)
	(compose f (repeated f (- n 1)))))

(define (repeated-fast f n)
  (cond ((= n 0) (lambda (x) x))
		((even? n) (repeated-fast (compose f f) (/ n 2)))
		(else (compose f (repeated-fast f (- n 1))))))

(define dx 0.0001)

(define (average x y z)
  (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (smooth-n f n)
  ((repeated-fast smooth n) f))
