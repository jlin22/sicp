(define (enumerate-interval s e)
  (if (> s e)
      ()
      (cons s (enumerate-interval (+ s 1) e))))

(define (pairs n)
  (accumulate append
	      ()
	      (map (lambda (i)
		     (map (lambda (j) (list i j))
			  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n))))

(define (accumulate p init seq)
  (if (null? seq)
      init
      (p (car seq) (accumulate p init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (pairs n))))

(define (prime? x)
  (define (helper i)
    (cond ((> (square i) x) #t)
	  ((= 0 (remainder x i)) #f)
	  (else (helper (+ i 1)))))
  (helper 2))
