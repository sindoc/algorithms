(defprotocol prot-a
  (op-a [self x y]))

(defprotocol prot-b
  (op-b [self x]))

(let [state (atom 10)]

  (deftype t-a [member]
    prot-a
    (op-a [self x y]
      (+ (.member self) x y))
    prot-b
    (op-b [self x]
      (+ (.member self) x @state))))

(def t-a-instance (t-a. 5))