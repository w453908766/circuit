
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;wire


(define (WIREX v)
	(if (wire? v) v (pin max-width v))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Ari
(define (CLA A B C-1)
	(if (= 1 (width A)) (unite (F G P) (xor-gate A B C-1) (and-gate A B) (or-gate A B))
	(split (F0 G0 P0) (CLA (lwhalf A) (lwhalf B) C-1)
	(split (F1 G1 P1) (CLA (hihalf-cl A) (hihalf-cl B) (or-gate G0 (and-gate P0 C-1)))
	(unite (F G P) (comb F1 F0) (or-gate G1 (and-gate P1 G0)) (and-gate P1 P0))
))))

(define (adder A B) ((CLA (align A B) (align B A) bit0) 'F))
(define (suber A B) ((CLA (align A B) (align (not-gate B) A) bit1) 'F)) 


(define (more A B) (high (suber B A)))

(define (EQU w0 w1) (apply and-gate (bit-list (xnor-gate w0 w1))))
(define (CMP g a b)
	(let ((A (WIREX a)) (B (WIREX b)))
	(cond ((eq? g '>) (more A B))
			((eq? g '=) (EQU A B))
			((eq? g '<)(more B A))
			(else bit0)
)))


(define (>> wire k)
	(if (= 0 (width k)) wire
	(let* ((wid (expt 2 (- (width k) 1)))
		(bw (- (width wire) wid)))
	(>> 
		(MIF (high k) (comb (subwire full0 0 wid) (subwire wire wid bw)) wire)
		(rmhigh k))
)))

(define (<< wire k)
	(if (= 0 (width k)) wire
	(let* ((wid (expt 2 (- (width k) 1)))
		(bw (- (width wire) wid)))
	(<<
		(MIF (high k) (comb (subwire wire 0 bw) (subwire full0 0 wid)) wire)
		(rmhigh k))
)))

(define (multier A B)
	(if (= 1 (width B)) (Cbuffer B A)
	(let0 hn (quotient (width B) 2)
	(let0 hf (multier A (hihalf B))
	(let0 lf (multier A (lwhalf B))
	(adderC (comb hf (subwire full0 0 hn))
			(comb (subwire full0 0 hn) lf) bit0)
)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Plexers

(define (-decoder code para)
	(if (= 0 (width code)) (apply and-gate para)
	(comb (-decoder (rmhigh code) (cons (high code) para))
		(-decoder (rmhigh code) (cons (not-gate (high code)) para))
)))
(define (decoder code) (-decoder code '()))

(define (Cbuffer e data) (and-gate data (extend e (width data))))
(define (MIF c w1 w0) (or-gate (Cbuffer (not-gate c) w0) (Cbuffer c w1)))

(define (encoder code)
	(if (= 1 (width code)) (struct (E C) code wnil)
	(split (E1 C1) (encoder (hihalf code))
	(split (E0 C0) (encoder (lwhalf code))
	(struct (E C) (or-gate E1 E0) (MIF E1 (comb bit1 C1) (comb bit0 C0)))
))))


(define (-MUX sel wires)
	(if (or (= 0 (width sel)) (null? wires)) '()
	(cons (Cbuffer (low sel) (car wires)) 
		(-MUX (rmlow sel) (cdr wires))
)))
(define (MUX code . wires) (apply or-gate (-MUX (decoder code) wires)))

(define (-DEMUX sel wire)
	(if (= 0 (width sel)) '()
	(cons (Cbuffer (low sel) wire)
		(-DEMUX (rmlow sel) wire)
)))

(define (DEMUX code wire) (-DEMUX (decoder code) wire))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;gate

(define (xnor-gate . wires) (not-gate (apply xor-gate wires)))