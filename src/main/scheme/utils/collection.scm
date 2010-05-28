(define (vector-map! vect proc)
  (define last (- (vector-length vect) 1))
  (let loop
    ((i 0))
    (vector-set! vect i (proc i (vector-ref vect i)))
    (if (< i last)
        (loop (+ i 1))))
  vect)