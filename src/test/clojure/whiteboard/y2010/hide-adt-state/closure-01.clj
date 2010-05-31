(ns com.khakbaz.algorithms.test.whiteboard.y2010)

(defn make-module []
  (let [state (atom 0)]
    (defn public-operation [x y]
      (println "I'm a public operation on this datatype.")
      (println "I consume the following arguments:" x y))
    (defn say-hello []
      (println "Hey Buddy!"))
    (fn [msg]
      (case msg
        :hello say-hello
        :pub public-operation))))

(def dt (make-module))
(def hello (dt :hello))
(def pub (dt :pub))
