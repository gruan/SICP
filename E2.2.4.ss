#lang racket

;;;Section 2.2.4 Exercises:

;Loads PLT Scheme SICP Picture Language package.
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;Context

;Makes painters split and branch towards the right.

(define (right-split painting n)
  (if (= n 0)
    painting
    (let ((smaller (right-split painting (- n 1))))
      (beside painting (below smaller smaller)))))

;Makes painters split and branch upwards as well as towards the right.

(define (corner-split painting n)
  (if (= n 0)
    painting
    (let ((up (up-split painting (- n 1)))
	  (right (right-split painting (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split painting (- n 1))))
	(beside (below painting top-left)
		(below bottom-right corner))))))

;Places four copies of corner-split appropriately to obtain a pattern.

(define (square-limit painting n)
  (let ((quarter (corner-split painting n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;Takes 4 one-argument painter operations and produces a painter operation that transforms a
;given painter with those 4 operations and arranges the results in a square. tl, tr, bl, br.

(define (square-of-four tl tr bl br)
  (lambda (painting)
    (let ((top (beside (tl painting) (tr painting)))
	  (bottom (beside (bl painting) (br painting))))
      (below bottom top))))

;Exercise 2.44

;Makes painters split and branch upwards.

(define (up-split painting n)
  (if (= n 0)
      painting
      (let ((smaller (up-split painting (- n 1))))
        (below painting (beside smaller smaller)))))

;Exercise 2.45

;Takes a painting as an argument and branches and splits the painting in a given direction.

(define (split branch-dir split-dir)
  (lambda (painting n)
    (if (= n 0)
      painting
      (let ((smaller ((split branch-dir split-dir) painting (- n 1))))
	(branch-dir painting (split-dir smaller smaller))))))

;Expressed as an instance of a general splitting operation.

(define right-split-template (split beside below))
(define up-split-template (split below beside))

;Exercise 2.46

;Creates a frame's coordinate map which is used to shift and scale images to fit the frame.
;The map transforms the unit square into the frame by mapping the vector v = (x,y) to the
;vector sum

;Origin(Frame) + x*Edge1(Frame) + y*Edge2(Frame)

(define (myframe-coord-map frame)
  (lambda (v)
    (vect-add
      (myframe-origin frame)
      (vect-add (vect-scale (vect-x v)
			    (myframe-edge1 frame))
		(vect-scale (vect-y v)
			    (myframe-edge2 frame))))))

;Vector constructor. Represents a vector running from the origin to a point (xcor, ycor).

(define (mymake-vect xcor ycor)
  (cons xcor ycor))

;Vector Selectors.

(define (vect-x vect) ;x-coordinate of the vector.
  (car vect))

(define (vect-y vect) ;y-coordinate of the vector.
  (cdr vect))

(define (vect-add vect1 vect2) ;Adds two vectors.
  (let ((new-x (+ (vect-x vect1) (vect-x vect2)))
	(new-y (+ (vect-y vect1) (vect-y vect2))))
    (mymake-vect new-x new-y)))

(define (vect-sub vect1 vect2) ;Subtracts two vectors, vect1-vect2.
  (let ((new-x (- (vect-x vect1) (vect-x vect2)))
	(new-y (- (vect-y vect1) (vect-y vect2))))
    (mymake-vect new-x new-y)))

(define (vect-scale c vect1) ;Scales a vector, vect1, by a constant, c.
  (let ((new-x (* c (vect-x vect1)))
	(new-y (* c (vect-y vect1))))
    (mymake-vect new-x new-y)))

;Exercise 2.47

;Frame constructors. Takes origin point, and 2 vector points as the edge of the frame.

(define (mymake-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (mymake-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;Frame selectors.

(define (myframe-origin frame) ;Returns origin point of the frame.
  (car frame))

(define (myframe2-origin frame) ;Returns origin point of the frame.
  (car frame))

(define (myframe-edge1 frame) ;Returns the vector, edge1's, point of the frame.
  (cadr frame))

(define (myframe2-edge1 frame) ;Returns the vector, edge1's, point of the frame.
  (cadr frame))

(define (myframe-edge2 frame) ;Returns the vector, edge2's, point of the frame.
  (caddr frame))

(define (myframe2-edge2 frame) ;Returns the vector, edge2's, point of the frame.
  (cddr frame))

;Exercise 2.48

;Directed line segment in the plane constructor.

(define (mymake-segment start end)
  (cons start end))

;Directed line segment in the plane selectors.

(define (mystart-segment segment) ;Starting point of the line.
  (car segment))

(define (myend-segment segment) ;Endpoint of the line.
  (cdr segment))

;Exercise 2.49

;a.

;Defines a primitive painter that draws the outline of the designated frame.

;Points of the frame.
(define A (make-vect 0.0 0.0))
(define B (make-vect 0.99 0.0))
(define C (make-vect 0.99 0.99))
(define D (make-vect 0.0 0.99))

;Segments of the frame.
(define AB (make-segment A B))
(define BC (make-segment B C))
(define CD (make-segment C D))
(define DA (make-segment D A))

(define outline-frame (list AB BC CD DA)) ;Segment list of the outline.

(define outline (segments->painter outline-frame)) ;Creates image of the frame.

;b

;Defines a primitive painter that draws an "X" by connecting opposite corners of the frame.

;Segments of the X.
(define AC (make-segment A C))
(define BD (make-segment B D))

(define X-segs (list AC BD)) ;Segment list of X.

(define X-pic (segments->painter X-segs)) ;Creates image of the X.

;c

;Defines a primitive painter that draws a diamond by connecting the midpoints of the sides of the frame.

;Points of the diamond.
(define mid-AB (make-vect 0.5 0.0))
(define mid-BC (make-vect 1.0 0.5))
(define mid-CD (make-vect 0.5 1.0))
(define mid-DA (make-vect 0.0 0.5))

;Segments of the diamond.
(define D1 (make-segment mid-AB mid-BC))
(define D2 (make-segment mid-BC mid-CD))
(define D3 (make-segment mid-CD mid-DA))
(define D4 (make-segment mid-DA mid-AB))

(define diamond-segs (list D1 D2 D3 D4)) ;Segment list of the diamond.

(define diamond-pic (segments->painter diamond-segs)) ;Creates image of the diamond.

;d

;Define the wave painter.

;Segments of wave.
(define w1 (make-segment (make-vect 0 0.85) (make-vect 0.15 0.6)))
(define w2 (make-segment (make-vect 0 0.65) (make-vect 0.16 0.4)))
(define w3 (make-segment (make-vect 0.15 0.6) (make-vect 0.31 0.65)))
(define w4 (make-segment (make-vect 0.16 0.4) (make-vect 0.31 0.6)))
(define w5 (make-segment (make-vect 0.31 0.65) (make-vect 0.4 0.65)))
(define w6 (make-segment (make-vect 0.31 0.6) (make-vect 0.36 0.5)))
(define w7 (make-segment (make-vect 0.25 0.0) (make-vect 0.36 0.5)))
(define w8 (make-segment (make-vect 0.35 0.85) (make-vect 0.4 0.65)))
(define w9 (make-segment (make-vect 0.35 0.85) (make-vect 0.4 1.0)))
(define w10 (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.3)))
(define w11 (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0.0)))
(define w12 (make-segment (make-vect 0.62 0.45) (make-vect 1.0 0.15)))
(define w13 (make-segment (make-vect 0.62 0.45) (make-vect 0.75 0.0)))
(define w14 (make-segment (make-vect 0.6 1.0) (make-vect 0.65 0.85)))
(define w15 (make-segment (make-vect 0.6 0.65) (make-vect 0.65 0.85)))
(define w16 (make-segment (make-vect 0.6 0.65) (make-vect 0.76 0.65)))
(define w17 (make-segment (make-vect 0.76 0.65) (make-vect 1.0 0.35)))

(define wave-segs (list w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17)) 
;Segment list of wave.

(define wave (segments->painter wave-segs)) ;Creates wave image.

;Exercise 2.50

;Transformation of a painter that flips it horizontally.

(define (myflip-horiz painting)
  (let ((origin (make-vect 1.0 0.0))
	(corner1 (make-vect 0.0 0.0))
	(corner2 (make-vect 1.0 1.0)))
    ((transform-painter origin corner1 corner2) painting)))

;Transformation of a painter that rotates the painter counterclockwise by 180 degrees.

(define (myrotate-180 painting)
  (let ((origin (make-vect 1.0 1.0))
	(corner1 (make-vect 0.0 1.0))
	(corner2 (make-vect 1.0 0.0)))
    ((transform-painter origin corner1 corner2) painting)))

;Transformation of a painter that rotates the painter counterclockwise by 180 degrees.

(define (myrotate-270 painting)
  (let ((origin (make-vect 0.0 1.0))
	(corner1 (make-vect 0.0 0.0))
	(corner2 (make-vect 1.0 1.0)))
    ((transform-painter origin corner1 corner2) painting)))

;Exercise 2.51

;Takes two painters as arguments. The resulting painter, given a frame, draws 
;with the first painter in the bottom of the frame and with the second painter on the top.

(define (mybelow p1 p2) ;Version 1.
  (let ((bot-painting ((transform-painter (make-vect 0.0 0.0)
					  (make-vect 1.0 0.0)
					  (make-vect 0.0 0.5))
		       p1))
	(top-painting ((transform-painter (make-vect 0.0 0.5)
					  (make-vect 1.0 0.5)
					  (make-vect 0.0 1.0))
		       p2)))
    (lambda (x)
      (bot-painting x)
      (top-painting x))))

(define (mybelow2 p1 p2) ;Version 2.
  (let ((bot (rotate90 p1))
	(top (rotate90 p2)))
    (rotate270 (beside top bot))))

;Exercise 2.52

;Make changes to the square limit of wave shown in figure 2.9 by working at different levels. In particular:

;a. Add some segments to the primitive wave painter.

;Added segments.
(define w18 (make-segment (make-vect 0.15 0.32) (make-vect 0.15 0.22)))
(define w19 (make-segment (make-vect 0.20 0.32) (make-vect 0.20 0.22)))
(define w20 (make-segment (make-vect 0.1 0.15) (make-vect 0.15 0.1)))
(define w21 (make-segment (make-vect 0.15 0.1) (make-vect 0.20 0.1)))
(define w22 (make-segment (make-vect 0.20 0.1) (make-vect 0.25 0.15)))

(define wave-segs-2 (list w18 w19 w20 w21 w22)) ;List of new wave segments

(define mod-wave-segs (append wave-segs wave-segs-2)) ;New wave list with original and added segments.

(define mod-wave (segments->painter mod-wave-segs)) ;Creates image for the new modified wave.

;b. Change the pattern constructed by corner-split by using only one copy of the up-split and right-split.

(define (mod-corner-split painting n)
  (if (= n 0)
    painting
    (let ((up (up-split painting (- n 1)))
	  (right (right-split painting (- n 1))))
      (let ((top-left up)
	    (bottom-right right)
	    (corner (corner-split painting (- n 1))))
	(beside (below painting top-left)
		(below bottom-right corner))))))

;c. Modify square-limit so as to assemble the corners in a different pattern.
;   Images are largest in the corner and they get progressively smaller towards the middle.

(define (mod-square-limit painting n)
  (let ((quarter (rotate180 (corner-split painting n))))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

