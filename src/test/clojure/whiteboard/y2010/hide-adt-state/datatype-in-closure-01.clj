(defprotocol prot-a
  (op-a [self x y]))

(let [state (atom 10)]
  (deftype t-a [member]
    prot-a
    (op-a [self x y]
      (+ (.member self) x y @state))))

(def t-a-instance (t-a. 5))