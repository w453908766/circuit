
(define gate-num 0)

;(vector strbit width (vector 'wire value func latency param spread extra))
(define (new-wire wid) (set! gate-num (+ 1 gate-num)) (vector 0 wid (vector 'wire 0 ret0 0 '() '() '())))
(define (wire-set! wire func latency param wires)
	(vector-set! (org-wire wire) 2 func)
	(vector-set! (org-wire wire) 3 latency)
	(vector-set! (org-wire wire) 4 param)
	(add-spread wire wires)
	(refresh wire)
	wire
)
(define (subwire wire h w)
  (vector (+ (strbit wire) h) w (org-wire wire))
)

(define (make-wire func wid latency wires)
	(let ((w (new-wire wid)))
	(wire-set! w func latency  wires wires)
))
(define (wire? x) 
	(if (not (vector? x)) #f
	(if (not (= (vector-length x) 3)) #f
	(let ((org (vector-ref x 2)))
	(if (not (vector? org)) #f
	(if (not (= (vector-length org) 7)) #f
	(eq? (vector-ref org 0) 'wire)
))))))
(define (strbit wire) (vector-ref wire 0))
(define (width wire) (vector-ref wire 1))
(define (org-wire wire) (vector-ref wire 2))
(define (org-value wire) (vector-ref (org-wire wire) 1))
(define (func wire) (vector-ref (org-wire wire) 2))
(define (latency wire) (vector-ref (org-wire wire) 3))
(define (param wire) (vector-ref (org-wire wire) 4))
(define (spread wire) (vector-ref (org-wire wire) 5))
(define (extra wire) (vector-ref (org-wire wire) 6))
(define (extra-set! wire x) (vector-set! (org-wire wire) 6 x)) 

(define (spread-cons! wire w) (vector-set! (org-wire wire) 5 (cons w (spread wire))))
(define (value-set! wire val) (vector-set! (org-wire wire) 1 val))

(define (value wire) (lownbit (ash (org-value wire) (- (strbit  wire))) (width wire)))

(define (low wire) (subwire wire 0 1))
(define (high wire) (subwire wire (- (width wire) 1) 1))
(define (rmlow wire) (subwire wire 1 (- (width wire) 1)))
(define (rmhigh wire) (subwire wire 0 (- (width wire) 1)))
(define (lwhalf wire) (subwire wire 0 (flr/ (width wire) 2)))
(define (hihalf wire) (subwire wire (ceil/ (width wire) 2) (flr/ (width wire) 2)))

(define (lwhalf-cl wire) (subwire wire 0 (ceil/ (width wire) 2)))
(define (hihalf-cl wire) (subwire wire (flr/ (width wire) 2) (ceil/ (width wire) 2)))


(define (wire-ref wire i) (subwire wire i 1))

(define (add-spread w wires)
  (if (null? wires) '()
      (begin
        (spread-cons! (car wires) w )
        (add-spread w (cdr wires))
)))





