(load "cirlisp.scm")

(define (full-adder a b c) 
	(comb (or-gate (and-gate a b) (and-gate a c) 
					(and-gate b c)) (xor-gate a b c))
)

(define (CRA A B C)
	(if (= 0 (width A)) C
	(let0 lwadd (full-adder (low A) (low B) (high C))
	(CRA (rmlow A) (rmlow B) (comb lwadd (rmhigh C)))
)))

(define (CSA A B C0)
	(if (= 1 (width A)) (full-adder A B C0)
	(let0 lwadd (CSA (lwhalf-cl A) (lwhalf-cl B) C0)
		(comb (MIF (high lwadd)  
				(CSA (hihalf A) (hihalf B) bit1)
				(CSA (hihalf A) (hihalf B) bit0))
			(rmhigh lwadd))
)))


(define a (pin 8 45))
(define b (pin 8 32))

(define z (CSA a b bit0))