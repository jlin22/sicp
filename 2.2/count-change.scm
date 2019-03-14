(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else
		  (+ (cc amount
				 (except-first-denomination coin-values))
			 (cc (- amount (first-denomination coin-values))
				 coin-values)))))

; Problem: How do you implement no-more?, except-first-denomination,
; first-denomination

(define (no-more? coin-values) (null? coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

(define (first-denomination coin-values) (car coin-values))

;; Problem: Does the order of the coins impact the run time of this algorithm?
;; Yes, there will be redundant checks if they are out of order,
;; Ex: (cc 90 (1 100)) will check (cc x (100)) for every x <= 90
;; (cc 90 (100 1)) will only check (cc 90 (100 1)) once

;; Consequently, sort the order of the coins from greatest to least

