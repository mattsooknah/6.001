#lang racket/gui

;; this is an implementation of ESCHER, the picture language described in SICP 2.2.4
;; in addition to the textbook material, I added a Sierpinski triangle painter

;; gory details of graphics implementation

(define size-x 400)
(define size-y 400)

(define picture (make-object bitmap% size-x size-y))
(define bm-dc (make-object bitmap-dc% picture))
(send bm-dc clear)

(define frame-gui (new frame% 
                       [label "Picture Language"]
                       [width (+ size-x 10)]
                       [height (+ size-y 35)]))

(define canvas (new canvas%
                    [parent frame-gui]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc draw-bitmap picture 0 0))]))

(define (draw-line v1 v2)
  (send bm-dc 
        draw-line 
        (* size-x (xcor-vect v1)) 
        (- size-y (* size-y (ycor-vect v1)))
        (* size-x (xcor-vect v2))
        (- size-y (* size-y (ycor-vect v2)))))

(send frame-gui show #t)

;; contract for VECT data structure

(define (make-vect xcor ycor) (list xcor ycor))
(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cadr vect))

;; some VECT operations

(define (add-vect v1 v2)
  (make-vect 
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect scale vect)
  (make-vect (* scale (xcor-vect vect))
             (* scale (ycor-vect vect))))

;; contract for FRAME data structure

(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

;; let's define our standard unit frame, for later use

(define frame 
  (make-frame (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              (make-vect 0.0 1.0)))

;; frame-coord-map takes a frame and outputs
;; an affine transformation on vectors
;; from the unit frame to the given frame
;; [frame -> (vector -> vector)]

(define (frame-coord-map frame)
  (lambda (v)                   
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; contract for SEGMENT data structure

(define (make-segment start end) (list start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cadr segment))

;; segments->painter takes a list of segments
;; and returns a function that draws those segments
;; in a frame (i.e. a PAINTER)
;; [(list of segments) -> (frame -> graphic)]

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; now that we can make PAINTERs, let's define some simple ones

;; note that a PAINTER is not a pure data structure! 
;; it is a PROCEDURE that maps one structure (a FRAME) to another (a GRAPHIC),
;; much like basic scheme procedures map one primitive type to another

(define blank
  (segments->painter
   empty))

(define square
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
         (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
         (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0)))))

(define cross
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

(define triangle
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.0))
         (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0)))))

(define diamond
  (segments->painter
   (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
         (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
         (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))

(define wave 
  (segments->painter 
   (list (make-segment (make-vect 0.25 0.00) (make-vect 0.37 0.37)) ;1
         (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.25)) ;2
         (make-segment (make-vect 0.50 0.25) (make-vect 0.62 0.00)) ;3
         (make-segment (make-vect 0.75 0.00) (make-vect 0.70 0.50)) ;4
         (make-segment (make-vect 0.70 0.50) (make-vect 1.00 0.30)) ;5
         (make-segment (make-vect 1.00 0.50) (make-vect 0.75 0.62)) ;6
         (make-segment (make-vect 0.75 0.62) (make-vect 0.62 0.62)) ;7
         (make-segment (make-vect 0.62 0.62) (make-vect 0.75 0.75)) ;8
         (make-segment (make-vect 0.75 0.75) (make-vect 0.62 1.00)) ;9
         (make-segment (make-vect 0.40 1.00) (make-vect 0.30 0.75)) ;10
         (make-segment (make-vect 0.30 0.75) (make-vect 0.40 0.62)) ;11
         (make-segment (make-vect 0.40 0.62) (make-vect 0.25 0.62)) ;12
         (make-segment (make-vect 0.25 0.62) (make-vect 0.20 0.50)) ;13
         (make-segment (make-vect 0.20 0.50) (make-vect 0.00 0.70)) ;14
         (make-segment (make-vect 0.37 0.37) (make-vect 0.30 0.50)) ;15
         (make-segment (make-vect 0.30 0.50) (make-vect 0.12 0.37)) ;16
         (make-segment (make-vect 0.12 0.37) (make-vect 0.00 0.50)) ;17
         )))

;; some higher-order operations on PAINTERs

;; transform-painter returns a modified PAINTER that draws
;; in a different frame from the original (in our case, the unit frame)
;; as specified by the new origin and corners

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

;; this can be used to implement many standard operations on images...

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

(define (squash-vert painter scale)
  (let ((margin (/ (- 1 scale) 2)))
    (transform-painter painter
                       (make-vect 0.0 margin)
                       (make-vect 1.0 margin)
                       (make-vect 0.0 (- 1 margin)))))

(define (squash-horiz painter scale)
  (let ((margin (/ (- 1 scale) 2)))
    (transform-painter painter
                       (make-vect margin 0.0)
                       (make-vect (- 1 margin) 0.0)
                       (make-vect margin 1.0))))

(define (squash-in painter scale)
  (squash-vert (squash-horiz painter scale) scale))

(define (slant-in painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


;; ...as well as more complicated ones.
;; COMPOUND PAINTERs are combinations of two or more basic painters,
;; and much like compound lists, they obey the closure property.
;; they are PAINTERs themselves, and thus our data abstraction is complete.

(define (overlay painter1 painter2)
  (lambda (frame) (painter1 frame) (painter2 frame)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((paint-bottom
         (transform-painter painter1
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 0.0)
                            (make-vect 0.0 0.5)))
        (paint-top
         (transform-painter painter2
                            (make-vect 0.0 0.5)
                            (make-vect 1.0 0.5)
                            (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

(define (duet painter1 painter2)
  (beside painter1 (flip-vert painter2)))

(define (flipped-pairs painter)
  (below (duet painter painter) (duet painter painter)))

;; our abstraction allows us to implement recursion very easily

(define (split transform1 transform2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((next ((split transform1 transform2) painter (- n 1))))
          (transform1 painter (transform2 next next))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; an implementation of the familiar Sierpinski triangle

(define sierpinski-starter (below (flip-vert (squash-horiz triangle (/ 1 2))) blank))

(define (trefoil painter)
  (below (beside painter painter) (squash-horiz painter (/ 1 2))))

(define (sierpinski painter n)
  (overlay painter
           (if (= n 0) painter
               (overlay (trefoil painter) (trefoil (sierpinski painter (- n 1)))))))

(define sierpinski-triangle (overlay triangle (sierpinski sierpinski-starter 5)))

(sierpinski-triangle frame)