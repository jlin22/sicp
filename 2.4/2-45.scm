(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside painter painter)))))

(define (split tr1 tr2)
  (define f (painter n)
    (if (= n 0)
	painter
	(let ((smaller (f painter (- n 1))))
	  (tr1 painter (tr2 smaller smaller)))))
  f)

(define right-split (split beside below))
(define up-split (split below beside))

