;(ns com.khakbaz.algorithms.memory-management.manual.pairs.pair)

(def null nil)
(def mem-size 5)
(def car-mem (ref nil))
(def cdr-mem (ref nil))
(def next-free 0)

(defn- init-mem []
  (let [mem (range 1 (+ mem-size 1))]
    (let [cars (vec mem)
          cdrs (vec (map (fn [_] null) mem))]
      (dosync
        (ref-set car-mem cars)
        (ref-set cdr-mem cdrs)
        nil))))

(defprotocol memory-manager
  (car [p])
  ;(car [pair])
  ;(cdr [pair])
  ;(set-car! [pair val])
  ;(set-cdr! [pair val])
  )

(defn xconss [car cdr]
  (cons car cdr))

(defn- car-impl [p]
  (.x p))

(defrecord pair [a b]
  memory-manager
  (car car-impl)
  

(init-mem)