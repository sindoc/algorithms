(ns com.khakbaz.algorithms.memory-management.manual.pairs.pair
  (:refer-clojure :exclude [cons]))

(def null nil)
(def mem-size 5)

(def car-mem (ref nil))
(def cdr-mem (ref nil))
(def next-free (ref 0))

(defn- set-car-mem [new-car-mem]
  (ref-set car-mem new-car-mem))
(defn- set-cdr-mem [new-cdr-mem]
  (ref-set cdr-mem new-cdr-mem))
(defn- set-next-free [new-next-free]
  (ref-set next-free new-next-free))

(defn- car-mem-set [idx new-val]
  (assoc @car-mem idx new-val))
(defn- cdr-mem-set [idx new-val]
  (assoc @cdr-mem idx new-val))

(defn- car-mem-ref [idx]
  (nth @car-mem idx))
(defn- cdr-mem-ref [idx]
  (nth @cdr-mem idx))

(defn- update-car-mem!
  [new-car-mem]
  (dosync
    (set-car-mem new-car-mem)))

(defn- update-car-mem-and-cdr-mem!
  [new-car-mem new-cdr-mem]
  (dosync
    (set-car-mem new-car-mem)
    (set-cdr-mem new-cdr-mem)))

(defn- update-car-mem-cdr-mem-and-next-free!
  [new-car-mem new-cdr-mem new-next-free]
  (dosync
    (set-car-mem new-car-mem)
    (set-cdr-mem new-cdr-mem)
    (set-next-free new-next-free)))

(defn- update-cdr-mem!
  [new-cdr-mem]
  (dosync
    (set-cdr-mem new-cdr-mem)))

(defn- update-car-mem-and-next-free!
  [new-car-mem new-next-free]
  (dosync
    (set-car-mem new-car-mem)
    (set-next-free new-next-free)))

(defn- init-mem []
  (let [mem (range 1 (+ mem-size 1))]
    (let [cars (vec mem)
          cdrs (vec (map (fn [_] null) mem))]
      (update-car-mem-and-cdr-mem! cars cdrs)
      null)))

(defprotocol manual-memory-manager
  "Memory should be freed by the programmer."
  (free [p]))

(defprotocol fixed-size-memory-manager
  "Manages fixed-size memory chunks,
  i.e. dotted pairs as opposed to vectors."
  (car [p] "Return the first element of the fixed structure.")
  (cdr [p] "Return the second element of the fixed structure.")
  (set-car! [p val] "Adapt the value of the first cell.")
  (set-cdr! [p val] "Adapt the value of the second cell."))

(deftype pair
  [addr]
  fixed-size-memory-manager
  (car [self]
    (car-mem-ref (.addr self)))
  (cdr [self]
    (cdr-mem-ref (.addr self)))
  (set-car! [self val]
    (update-car-mem!
      (car-mem-set (.addr self) val))
    null)
  (set-cdr! [self val]
    (update-cdr-mem!
      (cdr-mem-set (.addr self) val))
    null)
  manual-memory-manager
  (free [self]
    (update-car-mem-and-next-free!
      (car-mem-set (.addr self) @next-free)
      (.addr self))
    null))

(defn cons
  [car- cdr-]
  (let [addr @next-free]
    (update-car-mem-cdr-mem-and-next-free!
      (car-mem-set addr car-)
      (cdr-mem-set addr cdr-)
      (car-mem-ref addr))
    (pair. addr)))

(init-mem)

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