(load "pair.scm")

(define c1 (xcons 1 2))
(define c2 (xcons 3 4))
(define c3 (xcons c1 c2))
(display (list c1 c2 c3))
(newline)