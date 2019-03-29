(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))

(define (horner-eval x coeff-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* higher-terms x)))
	      0
	      coeff-sequence))
