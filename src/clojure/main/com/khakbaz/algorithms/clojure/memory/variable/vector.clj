(ns com.khakbaz.algorithms.clojure.memory.variable.vector)

(defprotocol variable-size-manual-memory-manager
  "Helps programmers free the previously acclaimed memory.
  To be merged with fixed-size-manual-memory-manager."
  (vfree [_]))

(defprotocol variable-size-memory-manager
  "Common operations provided by a variable size e.g. vectors
  memory manager"
  (vector-length [v])
  (vector-ref [v i])
  (vector-set! [v i val]))

(def null nil)