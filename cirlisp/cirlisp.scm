
(load "maclib.scm")
(load "cliplib.scm")
(load "funclib.scm")
(load "gate.scm")
(load "run.scm")
(load "wire.scm")

(define max-width 1024)
(define full0 (pin max-width 0))
(define full1 (pin max-width -1))
(define bit01 (pin max-width 1))
(define bit0 (pin 1 0))
(define bit1 (pin 1 1))
(define wnil (pin 0)) 


