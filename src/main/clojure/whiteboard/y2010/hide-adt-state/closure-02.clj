(declare public-op)

(let [state (atom 10)]
  (defn public-op
    [a b]
    (+ a b @state)))