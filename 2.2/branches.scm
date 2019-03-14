(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
	 (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
	(if (is-weight? structure)
	  structure
	  (total-weight structure))))

(define (is-weight? structure)
  (not (pair? structure)))

(define (balanced-mobile? mobile)
  (if (is-weight? mobile)
	#t
	(and (left-equal-right mobile)
	   (balanced-mobile? (branch-structure (left-branch mobile)))
	   (balanced-mobile? (branch-structure (left-branch mobile))))))

(define (left-equal-right mobile)
  (= (branch-torque (left-branch mobile))
	 (branch-torque (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-weight branch) (branch-length branch)))

; test case for functions
(define mobile2 (make-mobile (make-branch 4 7) (make-branch 4 7)))
; branch-weight mobile2 is supposed to be 14
(define mobile1 (make-mobile (make-branch 5 6) (make-branch 4 mobile2)))
; branch-weight mobile1 is supposed to be 14 + 6 = 20

; If we change our representation to use cons instead of list, all we need
; to change is the selectors right-branch, branch-structure

