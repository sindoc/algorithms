(ns com.khakbaz.algorithms.clojure.whiteboard.y2010.hide-adt-state.closure-02)

(declare public-op)

(let [state (atom 10)]
  (defn public-op
    [a b]
    (+ a b @state)))