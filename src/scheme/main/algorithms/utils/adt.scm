(define (xeval exp)
  (eval exp (interaction-environment))
  (eval exp (scheme-report-environment 5)))

(define-syntax export
  (syntax-rules ()
    ((export . exported)
     (for-each
      (lambda (def)
        (xeval def))
      (map 
       (lambda (binding)
         `(define ,binding ',binding))
       'exported)))))

(define-syntax bind
  (syntax-rules ()
    ((bind name proc)
     (xeval
      '(set! name proc)))))