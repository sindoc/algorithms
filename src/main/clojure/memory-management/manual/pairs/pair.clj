;(ns com.khakbaz.algorithms.memory-management.manual.pairs.pair)

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

(defn- update!
  ([new-car-mem]
    (dosync
      (set-car-mem new-car-mem)))
  ([new-car-mem new-cdr-mem]
    (dosync
      (set-car-mem new-car-mem)
      (set-cdr-mem new-cdr-mem)))
  ([new-car-mem new-cdr-mem new-next-free]
    (dosync
      (set-car-mem new-car-mem)
      (set-cdr-mem new-cdr-mem)
      (set-next-free new-next-free))))

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
      (update! cars cdrs)
      null)))

(defprotocol manual-memory-manager
  (free [p]))

(defprotocol fixed-size-memory-manager
  (car [p])
  (cdr [p])
  (set-car! [p val])
  (set-cdr! [p val]))

(deftype pair [addr]
  fixed-size-memory-manager
  (car [self]
    (car-mem-ref (.addr self)))
  (cdr [self]
    (cdr-mem-ref (.addr self)))
  (set-car! [self val]
    (update!
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

(defn xcons
  [car- cdr-]
  (let [addr @next-free]
    (update!
      (car-mem-set addr car-) ; new-car-mem
      (cdr-mem-set addr cdr-) ; new-cdr-mem
      (car-mem-ref addr)) ; new-next-free
    (pair. addr)))

(init-mem)

(def c (xcons 3 4))
(set-car! c (* (car c) 10))
(set-cdr! c (* (cdr c) 10))
(free c)

(def c1 (xcons 1 2))
(def c2 (xcons 3 4))
(def c3 (xcons c1 c2))
(free c3)
(free c1)
(free c2)