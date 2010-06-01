(ns com.khakbaz.algorithms.clojure.memory.fixed.pair)

(defprotocol fixed-size-manual-memory-manager
  "Memory should be freed by the programmer."
  (free [p]))

(defprotocol fixed-size-memory-manager
  "Manages fixed-size memory chunks,
  i.e. dotted pairs as opposed to vectors."
  (car [p] "Return the first element of the fixed structure.")
  (cdr [p] "Return the second element of the fixed structure.")
  (set-car! [p val] "Adapt the value of the first cell.")
  (set-cdr! [p val] "Adapt the value of the second cell."))

(def null nil)