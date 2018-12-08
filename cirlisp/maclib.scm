

(define-syntax unite
  (syntax-rules ()
    [(_ vars v0 ...)(lambda (var) (look-up var 'vars `(,v0 ...)))]
))
(define-syntax split
  (syntax-rules ()
    [(_ vars st body0 ...)
     (apply (lambda vars body0 ...) (st 1))]
))



(define-syntax let0
  (syntax-rules ()
    [(_ var val body0 ...) ((lambda (var) body0 ...) val)]
))
(define-syntax let1
  (syntax-rules ()
    [(_ vars vals body0 ...) (apply (lambda vars body0 ...) vals)]
))



(define (-sgl-list wires vals)
	(cond [(null? wires) 'OK]
		(else (sgl (car wires) (car vals)) (-sgl-list (cdr wires) (cdr vals)))
))
(define-syntax sgl-list
	(syntax-rules ()
	[(_ (w0  ...) vals) (-sgl-list `(,w0 ...) 'vals) ]
))

(define-syntax switch0
	(syntax-rules (else)
		[(_ wire) full0]
		[(_ wire (else r)) (WIREX r)] 
		[(_ wire (v0 r0) (v1 r1) ...) (MIF (EQU wire (WIREX v0)) (WIREX r0) (switch0 wire (v1 r1) ...))]
))

(define (sw-mat wires vals)
	(if (null? wires) bit1
	(if (eq? (car vals) '_) (sw-mat (cdr wires) (cdr vals))
	(and-gate (EQU (car wires) (WIREX (car vals))) (sw-mat (cdr wires) (cdr vals)))
)))

(define-syntax switch1
	(syntax-rules (else)
		[(_ wires) full0]
		[(_ wires (else r)) (WIREX r)]
		[(_ (w0 ...) ((v0 ...) r0) (v1 r1) ...) (MIF (sw-mat `(,w0 ...) `(,v0 ...)) (WREX r0)
													(switch1 (w0 ...) (v1 r1) ...))]
))  

(define-syntax loop
	(syntax-rules()
	[(_ ((w0 wid0) (w1 wid1) ...) exp0 exp1 ...)
		(let0 w0 (pin wid0)
		(loop ((w1 wid1) ...) exp1 ... (link-wire w0 exp0)))]
	[(_ () exp exp0 ...) (begin exp0 ... exp)]	
))


