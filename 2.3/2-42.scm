(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queens-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (empty-row (- k 1))))
  
(define (empty-row size)
  (if (= size 0)
      ()
      (cons 0 (empty-row (- size 1)))))

(define empty-board (list ()))
