

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;pure-func
(define flr/ quotient)
(define (ceil/ a b) (- a (flr/ a b)))


(define (lognand a b) (lognot (logand a b)))
(define (lognor a b) (lognot (logor a b)))

(define (nbit1 wid)  (- (ash 1 wid) 1))
(define (lownbit n wid) (logand n (nbit1 wid)))

(define (ret0 k) 0)
(define (delay-time t)
	(if (= t 0) 'OK
	(delay-time (- t 1))
))
(define (make-list w k)
	(if (= k 0) '()
	(cons w (make-list w (- k 1)))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;vals
(define (val2 wire) (number->string (value wire) 2))
(define (val8 wire) (number->string (value wire) 8))
(define (val16 wire) (number->string (value wire) 16))




(define (-look-up var vars vals)
	(cond ((null? vars) '())
		((eq? var (car vars)) (car vals))
		(else (look-up var (cdr vars) (cdr vals)))
))

(define (look-up var vars vals)
  (cond ((eq? var '0) vars)
        ((eq? var 1) vals)
		((-look-up var vars vals))
))

(define (-vals wires)
	(cond ((null? wires) '())
		((wire? wires) (val16 wires))
		((pair? wires) (cons (-vals (car wires)) (-vals (cdr wires))))
		((procedure? wires) (-vals (wires 1)))
		((vector? wires) (-vals (vector->list wires)))
		(else 'other)
))
(define (vals . wires) (-vals wires))
(define (align . wires) (subwire (car wires) 0 (ws-wid wires)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;btree

(define (bt->list tree lst)
	(if (not (pair? tree)) (cons tree lst)
	(bt->list (car tree) (bt->list (cdr tree) lst))
))
(define (-list->bt lst)
	(if (null? lst) '()
	(if (null? (cdr lst)) lst
	(cons (cons (car lst) (cadr lst)) (-list->bt (cddr lst)))
)))
(define (list->bt lst)
	(if (null? (cdr lst)) (car lst)
	(list->bt (-list->bt lst))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;vector
(define (mem-set! mem addr . vals)
	(vec-set! (extra mem) addr vals)
	(refresh mem)
)
(define (mem-check mem addr) (vec-check (extra mem) addr 32))


(define (vec-set! vec addr vals)
	(if (null? vals) 'OK
	(begin (vector-set! vec addr (car vals))
			(vec-set! vec (+ addr 1) (cdr vals))
)))
(define (vec-check vec addr n)
	(cond ((= n 0) '())
		((>= addr (vector-length vec)) '())
		(else (let ((lst (cons (number->string (vector-ref vec addr) 16)
						(vec-check vec (+ addr 1) (- n 1)))))
				(if (= 0 (remainder addr 4)) 
					(cons (string-append "(" (number->string addr 16) ")") lst)
					lst))
)))


(define (-vec-map vec1 f vec0 i)
	(if (= i (vector-length vec0)) 'OK
	(begin (vector-set! vec1 i (f vec0 i))
		(-vec-map vec1 f vec0 (+ i 1))
)))
	
(define (vec-map vec f) 
	(let ((v (make-vector (vector-length vec))))
	(-vec-map v f vec 0)
	v)
)
(define (vec-init vec f i)
	(if (= i (vector-length vec)) 'OK
	(begin (vector-set! vec i (f i))
	(vec-init vec f (+ i 1))
)))
(define (make-vec n f)
	(let ((v (make-vector n)))
	(vec-init v f 0)
	v)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;queue
(define (make-queue)
	(let ((tail (cons #f '())))
	(cons tail tail)
))

(define (dequeue que)
	(let ((lst (car que)))
	(set-car! que (cdr lst))
	(car lst)
))
(define (enqueue que val)
	(let ((tail (cdr que)))
	(set-car! tail val)
	(set-cdr! tail (cons #f '()))
	(set-cdr! que (cdr tail))
))
(define (empty-que? que)
	(null? (cdar que))
)