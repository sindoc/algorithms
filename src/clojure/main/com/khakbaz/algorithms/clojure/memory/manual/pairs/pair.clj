(ns com.khakbaz.algorithms.clojure.memory.manual.pairs.pair
  (:refer-clojure :exclude [cons]))

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

(deftype pair [addr])

(def null nil)
(def mem-size 5)

(def car-mem (ref nil))
(def #^{:private true} cdr-mem (ref nil))
(def #^{:private true} next-free (ref 0))

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
    (ref-set car-mem new-car-mem)))

(defn- update-car-mem-and-cdr-mem!
  [new-car-mem new-cdr-mem]
  (dosync
    (ref-set car-mem new-car-mem)
    (ref-set cdr-mem new-cdr-mem)))

(defn- update-car-mem-cdr-mem-and-next-free!
  [new-car-mem new-cdr-mem new-next-free]
  (dosync
    (ref-set car-mem new-car-mem)
    (ref-set cdr-mem new-cdr-mem)
    (ref-set next-free new-next-free)))

(defn- update-cdr-mem!
  [new-cdr-mem]
  (dosync
    (ref-set cdr-mem new-cdr-mem)))

(defn- update-car-mem-and-next-free!
  [new-car-mem new-next-free]
  (dosync
    (ref-set car-mem new-car-mem)
    (ref-set next-free new-next-free)))

(defn- init-mem []
  (let [mem (range 1 (+ mem-size 1))]
    (let [cars (vec mem)
          cdrs (vec (map (fn [_] null) mem))]
      (update-car-mem-and-cdr-mem! cars cdrs)
      null)))

(defn car
  [self]
  (car-mem-ref (.addr self)))

(defn cdr
  [self]
  (cdr-mem-ref (.addr self)))

(defn set-car!
  [self val]
  (update-car-mem!
    (car-mem-set (.addr self) val))
  null)

(defn set-cdr!
  [self val]
  (update-cdr-mem!
    (cdr-mem-set (.addr self) val))
  null)

(defn free
  [self]
  (update-car-mem-and-next-free!
    (car-mem-set (.addr self) @next-free)
    (.addr self))
  null)

(defn cons
  [car- cdr-]
  (let [addr @next-free]
    (update-car-mem-cdr-mem-and-next-free!
      (car-mem-set addr car-)
      (cdr-mem-set addr cdr-)
      (car-mem-ref addr))
    (pair. addr)))

(extend pair
  fixed-size-memory-manager
  {:car car
   :cdr cdr
   :set-car! set-car!
   :set-cdr! set-cdr!}
  manual-memory-manager
  {:free free})

(init-mem)