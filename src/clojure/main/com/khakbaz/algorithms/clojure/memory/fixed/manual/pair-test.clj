(ns com.khakbaz.algorithms.clojure.memory.fixed.manual.test-pair
  (:require [com.khakbaz.algorithms.clojure.memory.fixed.manual.pair :as p]))

(def c (p/cons 3 4))
(p/set-car! c (* (p/car c) 10))
(p/set-cdr! c (* (p/cdr c) 10))
(p/free c)

(def c1 (p/cons 1 2))
(def c2 (p/cons 3 4))
(def c3 (p/cons c1 c2))

(p/free c3)
(p/free c1)
(p/free c2)