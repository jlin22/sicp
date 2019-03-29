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
      
