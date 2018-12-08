(load "cirlisp.scm")


(define (trans STATE OPR)
	(MUX STATE
		(WIREX 1) (WIREX 2) (WIREX 3)
		(WIREX 4) (WIREX 5) (WIREX 6)
		(switch0 OPR (#xF 9) (#xE 11) (else 7))
		(WIREX 8) (WIREX 0) 
		(WIREX 10) (WIREX 0)
		(WIREX 0)
))

(define (ALU OPR OPD1 OPD2)
	(MUX OPR
		(adder OPD1 OPD2)			;0
		(suber OPD1 OPD2)			;1
		(and-gate OPD1 OPD2)				;2
		(or-gate OPD1 OPD2)				;3
		(xor-gate OPD1 OPD2)				;4
		(<< OPD1 (subwire OPD2 0 4));5
		(>> OPD1 (subwire OPD2 0 4));6
		(comb (subwire full0 0 15) (EQU OPD1 OPD2))		;7
		(comb (subwire full0 0 15) (more OPD1 OPD2))	;8
))

(define E (pin 1))
(define STATE (pin 4))
(define ANS (pin 16))
(define IR1 (pin 16))
(define IR2 (pin 16))
(define OPD1 (pin 16))
(define IP (pin 14))
(define ADR (pin 14))

(define DEC (decoder STATE))
(define MEM (memory ADR OPD1 (and-gate E (wire-ref DEC 10))))
(define OPR (comb (subwire IR1 14 2) (subwire IR2 14 2)))
(define OPD2 (subwire IR2 0 14))
(define BUS (MIF (CMP '= ADR #x3FFF) ANS MEM))

(link-wire STATE (reg (trans STATE OPR) (not-gate E)))
(link-wire ANS (reg (ALU OPR OPD1 BUS) (and-gate E (wire-ref DEC 8))))
(link-wire IR1 (reg MEM (and-gate E (wire-ref DEC 2))))
(link-wire IR2 (reg MEM (and-gate E (wire-ref DEC 4))))
(link-wire OPD1 (reg BUS  (and-gate E (wire-ref DEC 6))))
(link-wire IP (reg (MIF (wire-ref DEC 11) OPD2 (adder IP bit01))
						(and-gate E (or-gate (wire-ref DEC 2) (wire-ref DEC 4)
						(and-gate (wire-ref DEC 11) (CMP '= OPD1 full0))))))
(link-wire ADR (reg (MIF (or-gate (wire-ref DEC 1) (wire-ref DEC 3)) IP 
					(MIF (wire-ref DEC 5) (subwire IR1 0 14) OPD2))
					(and-gate E (or-gate (wire-ref DEC 1) (wire-ref DEC 3) 
					(wire-ref DEC 5) (wire-ref DEC 7) (wire-ref DEC 9)))))
					





(probe 'IP IP)
(probe 'ANS ANS)
;(probe 'STATE STATE)


(define (step)
	(press E)
	(if (not (= 0 (value STATE))) (step) 'OK)
)
(define (go)
	(if (= (value IP) 14) 'OK
	(begin
		(press E)
		(go)
)))


;(mem-set! MEM 0 #xC00F #x800C #x000F #x4012 #xFFFF #xC00F #x0010 #x000E 
;						#xFFFF #xC010 #xC011 #x8000 #xC011 #x800C 5 3 0 0 1)		;mul
						
(mem-set! MEM 0 #xC00F #x800C #x000E #x000F #xFFFF #xC00E #x000F #x4010 
				#xFFFF #xC00F #xC011 #x8000 #xC011 #x800C 0 5 1 0) 





;1	ADR<-IP
;2	IR1<-MEM
;	IP<-IP+1
;3	ADR<-IP
;4	IR2<-MEM
;	IP<-IP+1
;5	ADR<-IR1
;6	OPD1<-MEM
;ALU	
;7	ADR<-OPD2
;8	ANS<-ALU
;MOV
;9	ADR<-OPD2
;10	MEM<-OPD1
;IF
;11	if(opd1=0)IP<-OPD2

;sigma
;if(n==0)goto finish	;start
;+ a n
;mov ans a
;- n 1
;mov ans n
;goto start
;goto finish		;finish
;var a,n
;var num1,num0



;mul
;if(b==0)goto finish	;start
;- b 1
;mov ans b
;+ c a
;mov ans c
;goto start
;goto finish		;finish
;var a,b
;var c,num0
;var num1



