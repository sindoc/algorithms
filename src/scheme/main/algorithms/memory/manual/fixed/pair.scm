(load "../../../utils/collection.scm")
(load "../../../utils/adt.scm")

(define *manual-pairs-memory-size* 5)
(define null '())

(export xpair? xfree xcar xcdr xset-car! xset-cdr!)

(define (xcons car- cdr-)
  
  (define tag 'pair)
  (define mem-size *manual-pairs-memory-size*)
  
  (let* ((car-mem (make-vector mem-size null))
         (cdr-mem (make-vector mem-size null))
         (next-free 0))

    (define (make addr)
      (cons tag addr))
    
    (define (address pair)
      (if (not (pair? pair))
          (error "expected a pair, given" pair))
      (cdr pair))
    
    (vector-map! car-mem (lambda (i _) (+ i 1)))
    
    (display car-mem)
    (newline)
    (display cdr-mem)
    
    ;; (pair? any) -> boolean
    (set!
     xpair?
     (lambda (any)
       (and (pair? any)
            (eq? (car any) tag))))
    
    ;; (xcons any any) -> pair
    (set! 
     xcons
     (lambda (car- cdr-)
       (define addr next-free)
       (if (eq? addr null)
           (error "Out of storage space" xcons))
       (set! next-free (vector-ref car-mem addr))
       (vector-set! car-mem addr car-)
       (vector-set! cdr-mem addr cdr-)
       (make addr)))
    
    ;; (xfree pair) -> undefined
    (set!
     xfree
     (lambda (pair)
       (define addr (address pair))
       (vector-set! car-mem addr next-free)
       (set! next-free addr)))
    
    ;; (xcar pair) -> any
    (set!
     xcar
     (lambda (pair)
       (display car-mem)
       (vector-ref car-mem (address pair))))
    
    ;; (xcdr pair) -> any
    (set!
     xcdr
     (lambda (pair)
       (vector-ref cdr-mem (address pair))))
    
    ;; (xset-car! pair any) -> undefined
    (set!
     xset-car!
     (lambda (pair any)
       (vector-set! car-mem (address pair))))
    
    ;; (xset-cdr! pair any) -> undefined
    (set!
     xset-cdr!
     (lambda (pair any)
       (vector-set! cdr-mem (address pair))))
    
    (xcons car- cdr-)))