(ns com.khakbaz.algorithms.clojure.whiteboard.y2010.hide-adt-state.datatype-and-closure-01)

(declare op-a)

(defprotocol prot-a
  (op-a [self x y]))

(deftype t-a [member])

(let [state (atom 10)]
  (defn op-a
    [this x y]
    (+ (.member this) x y @state))

  (extend t-a
    prot-a
    {:op-a op-a}))

(def t-a-1 (t-a. 5))