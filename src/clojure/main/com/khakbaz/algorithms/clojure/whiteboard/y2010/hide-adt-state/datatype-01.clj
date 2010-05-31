(ns com.khakbaz.algorithms.clojure.whiteboard.y2010.hide-adt-state.datatype-01)

(def #^{:private true} state (atom 10))

(defprotocol prot-a
  (op-a [self x y]))

(deftype t-a [member]
  prot-a
  (op-a [self x y]
    (+ (.member self) x y @state)))

(def t-a-instance (t-a. 5))