(ns com.khakbaz.algorithms.clojure.whiteboard.y2010.hide-adt-state.datatype-and-closure-01)

(defprotocol prot-a
  (op-a [self x y]))

(let [state (atom 10)]
  (defn op-a
    [self x y]
    (+ x y (.member self)  @state)))

(deftype t-a [member]
  prot-a
  (op-a [self x y]
    (op-a self x y)))

(def t-a-1 (t-a. 5))