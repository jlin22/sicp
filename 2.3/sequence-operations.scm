(define (filter pred sequence)
  (cond ((null? sequence) ())
	((pred (car sequence)) (cons (car sequence)
				     (filter pred (cdr sequence))))
	(else (filter pred (cdr sequence)))))

(define (enumerate-interval start end)
  (if (> start end)
      ()
      (cons start (enumerate-interval (+ start 1) end))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map op sequence)
  (if (null? sequence)
      ()
      (cons (op (car sequence)) (map op (cdr sequence)))))

(define (is-leaf? tree) (not (pair? tree)))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
	((is-leaf? tree) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

;; sum odd squares and even fibs

(define (sum-odd-squares n)
  (accumulate + 0
	      (filter odd?
		      (map square
			   (enumerate-interval 1 n)))))

(define (even-fibs n)
  (filter even?
	  (map fib
	       (enumerate-interval 1 n))))

(define (fib n)
  (define (fib-helper a b count) ;; count represents which fib number b is
    (if (= count n)
	b
	(fib-helper (+ a b) a (+ count 1))))
  (fib-helper 1 1 1))
