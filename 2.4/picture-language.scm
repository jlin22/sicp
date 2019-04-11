(define wave2 (beside wave (flip-vert wave)))

;(define wave4 (below wave2 wave2))
	       
(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;(define (flipped-pairs painter)
;  (let ((painter2 (beside painter (flip-vert wave))))
;    (below painter2 painter2)))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (split trans1 trans2)
  (lambda (painter)
    (trans1 painter (trans2 painter painter))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v w) (make-vect (+ (xcor-vect v) (ycor-vect w))
				  (+ (xcor-vect v) (ycor-vect w))))
(define (scale-vect s v) (make-vect (* s (xcor-vect v))
				    (* s (ycor-vect v))))
(define (sub-vect v w) (add-vect v (scale-vect -1 w)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (make-segment s e)
  (cons s e))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (draw-frame frame)
  (let ((diagonal (add-vect (edge1-frame frame) (edge2-frame frame))))
    (segments->painter (list (make-segment (origin-frame frame) (edge1-frame frame))
			     (make-segment (origin-frame frame) (edge2-frame frame))
			     (make-segment (edge1-frame frame) diagonal)
			     (make-segment (edge2-frame frame) diagonal)))))
(define (draw-X frame)
  (let ((diagonal (add-vect (edge1-frame frame) (edge2-frame frame))))
    (segments->painter (list (make-segment (edge1-frame frame) (edge2-frame frame))
			     (make-segment (origin-frame frame) diagonal)))))
(define (draw-diamond frame)
  (let ((diagonal (add-vect (edge1-frame frame) (edge2-frame frame))))
    (let ((mp1 (scale-vect 0.5 (add-vect (origin-frame frame) (edge1-frame frame))))
	  (mp2 (scale-vect 0.5 (add-vect (origin-frame frame) (edge2-frame frame))))
	  (mp3 (scale-vect 0.5 (add-vect (edge2-frame frame) diagonal)))
	  (mp4 (scale-vect 0.5 (add-vect (edge1-frame frame) diagonal))))
      (segments->painter (list (make-segment mp1 mp2)
			       (make-segment mp2 mp3)
			       (make-segment mp3 mp4)
			       (make-segment mp4 mp1))))))

			   

       
