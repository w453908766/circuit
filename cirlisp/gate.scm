
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;wire
(define (comb-wid wires)
  (if (null? wires) 0
      (+ (width (car wires)) (comb-wid (cdr wires)))
))


(define (comb . wires) (make-wire comb-func (comb-wid wires) 0 wires))
(define pin (case-lambda
	[(wid) (new-wire wid)]
	[(wid val) (let ((wire (new-wire wid))) (sgl wire val) wire)]
))

(define (link-wire w0 w1)
	(wire-set! w0 value 0 (list w1) (list w1))
	'OK
)

(define (bit-list wire)
	(if (= 0 (width wire)) '()
	(cons (low wire) (bit-list (rmlow wire)))
))

(define (extend-func wire k n)
	(if (= k 0) n
	(extend-func wire (- k 1) (logor (ash n (width wire)) (value wire)))
))	


(define (extend wire k)
	(let ((w (new-wire (* k (width wire)))))
		(wire-set! w extend-func 0 (list wire k 0) (list wire))
		w)
)
(define (press button)
	(sgl button 0)
	(sgl button 1)
	;(sgl button 0)
)

(define (probe-func name w wire) 
		(printf "~s ~d -> ~d\n" name (value w) (value wire))
		(value wire)
)
(define (probe name wire)
	(let ((w (new-wire (width wire))))
		(wire-set! w probe-func 0 (list name w wire) (list wire))
		'OK
))

(define (delayer t wire) (make-wire value (width wire) t (list wire)))

(define (-comb-func wires val)
	(if (null? wires) val
		(-comb-func (cdr wires) (logor (value (car wires)) (ash val (width (car wires)))))
))
(define (comb-func . wires)
	(-comb-func wires 0)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;gate

(define (ws-wid wires)
	(if (null? (cdr wires)) (width (car wires))
	(min (width (car wires)) (ws-wid (cdr wires)))
))

(define (gate-func op wires)
  (if (null? (cdr wires)) (value (car wires))
      (op (value (car wires)) (gate-func op (cdr wires)))
))
(define (and-func . wires) (gate-func logand wires))
(define (or-func . wires) (gate-func logor wires))
(define (xor-func . wires) (gate-func logxor wires))
(define (not-func wire) (lognot (value wire)))


(define (and-gate . wires) (make-wire and-func (ws-wid wires) 2 wires))
(define (or-gate . wires) (make-wire or-func (ws-wid wires) 2 wires))
(define (xor-gate . wires) (make-wire xor-func (ws-wid wires) 5 wires))

(define (nand-gate . wires) (apply not-gate (apply and-gate wires)))
(define (nor-gate . wires) (apply not-gate (apply or-gate wires)))

(define (not-gate . wires) (make-wire not-func (ws-wid wires) 1 wires))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;memory

(define (reg-func r data trg)
	(if (eq? (value trg) 1)  (value data) (value r))
)
(define (reg data trg)
	(let ((r (new-wire (width data))))
		(wire-set! r reg-func 6 (list r data trg) (list trg))
		r
))


(define (mem-func addr data trg arr)
	(if (eq? (value trg) 1) (vector-set! arr (value addr) (value data)))
	(vector-ref arr (value addr))
)
(define (memory addr data trg)
	(let ((wire (new-wire (width data))))
		(extra-set! wire (make-vector (ash 1 (width addr)) 0))
		(wire-set! wire mem-func 32 (list addr data trg (extra wire)) (list addr trg))
		wire
))

