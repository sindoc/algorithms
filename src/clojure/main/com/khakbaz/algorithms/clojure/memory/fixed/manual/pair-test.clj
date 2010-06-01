(ns com.khakbaz.algorithms.clojure.memory.fixed.manual.test-pair
  (:use com.khakbaz.algorithms.clojure.memory.fixed.manual.pair)
  (:refer-clojure :exclude [cons]))

(def c (cons 3 4))
(set-car! c (* (car c) 10))
(set-cdr! c (* (cdr c) 10))
(free c)

(def c1 (cons 1 2))
(def c2 (cons 3 4))
(def c3 (cons c1 c2))

(free c3)
(free c1)
(free c2)