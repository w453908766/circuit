(define time-cycle 500)
(define timebase (make-vec time-cycle (lambda (i) (make-queue)))) 

(define (update wire)
	(let ((a (apply (func wire) (param wire))))
		(cond ((= a (value wire)) '())
		(else (value-set! wire a) (spread wire))
)))
(define (insert-list lst k vtime)
	(if (null? lst) k
	(let ((ut (remainder (+ vtime (latency (car lst))) time-cycle)))
	(enqueue (vector-ref timebase ut) (car lst))
	(insert-list (cdr lst) (+ k 1) vtime)
)))
(define (update-queue que n vtime)
	(if (empty-que? que) n
	(update-queue que 
				(+ n -1 (insert-list (update (dequeue que)) 0 vtime)) 
				vtime
)))


(define (run n time)
	(if (= n 0) (- time 1)
	(let ((vtime (remainder time time-cycle)))
	(run (update-queue (vector-ref timebase vtime) n vtime) (+ time 1))
)))

(define (sgl wire val)
	(if (= val (value wire)) 'OK 
	(begin 
		(value-set! wire val)
		(run (insert-list (spread wire) 0 0) 0)
)))

(define (refresh wire)
	(sgl wire (apply (func wire) (param wire)))
)