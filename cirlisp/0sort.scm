(load "cirlisp.scm")

(define (CMP> A B)
	(if (= 0 (width A)) bit0
	(MIF (and-gate (high A) (not-gate (high B))) bit1
	(MIF (and-gate (not-gate (high A)) (high B)) bit0
	(CMP> (rmhigh A) (rmhigh B))
))))


(define (CSWAP vec a b)
	(let0 A (vector-ref vec a)
	(let0 B (vector-ref vec b)
	(let0 m (CMP> A B)
	(vector-set! vec a (MIF m B A))
	(vector-set! vec b (MIF m A B))
))))


(define (half-cleaner vec a n i)
	(if (= i (/ n 2)) 'OK
	(begin
	(CSWAP vec (+ a i) (+ a i (/ n 2)))
	(half-cleaner vec a n (+ i 1))
)))

(define (bitonic vec a n)
	(if (= n 1) 'OK
	(begin
	(half-cleaner vec a n 0)
	(bitonic vec a (/ n 2))
	(bitonic vec (+ a (/ n 2)) (/ n 2))
)))

 
(define (-MERGER vec a n)
	(if (= n 0) 'OK
	(begin
	(CSWAP vec a (+ a (- n 1)))
	(-MERGER vec (+ a 1) (- n 2))
)))
	
	
(define (SORTER vec a n)
	(if (= 1 n) 'OK
	(begin
	(SORTER vec a (/ n 2))
	(SORTER vec (+ a (/ n 2)) (/ n 2))
	(-MERGER vec a n)
	(bitonic vec a n)
)))

(define (order vec i)
	(if (= i (- (vector-length vec) 1)) #t
	(if (> (value (vector-ref vec i)) (value (vector-ref vec (+ i 1)))) #f
	(order vec (+ i 1))
)))

(define (rand-wires vec) (vec-map vec (lambda (v i) (sgl (vector-ref v i) (random 60000)))))
(define (make-wires n) (make-vec n (lambda (i) (pin 16))))

(define n (expt 2 3))
(define inp (make-wires n))
(define outp (vec-map inp vector-ref))
 (rand-wires inp) 

(SORTER outp 0 n)
(display (order inp 0))
(display (order outp 0))


