(defprotocol p
  (m
    [_ a]
    [_ a b]))

(deftype t [])

(extend t p
  {:m
  (fn
    ([_ a]
      (println a))
    ([_ a b]
      (println a b)))
   })

(def t-1 (t.))
