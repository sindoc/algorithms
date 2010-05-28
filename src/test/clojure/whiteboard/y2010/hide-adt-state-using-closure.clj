(ns com.khakbaz.algorithms.test.whiteboard.y2010)

(def public-a nil)

(binding
  [state 0]

  (set! public-a
    (fn [x]
      (println "I'm a public member of this adt.")))

  (println public-a)
  )