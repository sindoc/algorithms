(ns com.khakbaz.algorithms.clojure.memory.variable.vector)

(defprotocol variable-size-manual-memory-manager
  "Helps programmers free the previously acclaimed memory.
  To be merged with fixed-size-manual-memory-manager."
  (vfree [_]))

(defprotocol variable-size-memory-manager
  "Common operations provided by a variable size e.g. vectors
  memory manager"
  (vlength [v])
  ;(vref [v i])
  ;(vset! [v i val])
  )

(def null nil)