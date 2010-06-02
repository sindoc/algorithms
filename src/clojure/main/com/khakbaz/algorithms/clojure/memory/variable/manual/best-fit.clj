(ns com.khakbaz.algorithms.clojure.memory.variable.manual.best-ft
  (:import java.util.Random)
  (:use [com.khakbaz.algorithms.clojure.memory.variable.vector
         :only [null variable-size-memory-manager
                variable-size-manual-memory-manager]])
  (:refer-clojure :rename {peek clojure-peek vector clojure-vector}))

(deftype vector [addr])

(def min-size 5)
(def mem-size 20)

(defprotocol binary-tree-navigator
  (left [_ node])
  (left! [_ node val])
  (right [_ node])
  (right! [_ node val]))

(def mem #^{:private true} (ref nil))

(defn- peek [addr]
  (nth @mem addr))

(defn- poke [addr val]
  (assoc @mem addr val))

(defn- poke![addr val]
  (dosync
    (ref-set mem (poke addr val))))

(deftype weird-tree [root offset])

(extend weird-tree
  binary-tree-navigator
  {:left
    (fn [self addr]
      (peek (+ (.offset self) addr)))
   :left!
    (fn [self addr val]
      (poke! (+ addr (.offset self) val)))
   :right
    (fn [self addr]
     (peek (+ addr (.offset self) 2)))
   :right!
    (fn [self addr val]
     (poke! (+ addr (.offset self) 2)))})

(def size-tree #^{:private true} (ref (weird-tree. 10 1)))
(def addr-tree #^{:private true} (ref (weird-tree. 10 3)))

(defn- update-mem! [new-mem]
  (dosync (ref-set mem new-mem)))

(defn- size[addr]
  (peek addr))

(defn- size! [addr new-size]
  (poke! addr new-size))

(defn- find-addr [addr]
  (letfn
    [(rec [top]
      (cond
        (= top null) null
        (= top addr) top
        (> top addr)
          (rec (left @addr-tree top))
        :else
        (let [try (rec (right @addr-tree top))]
          (if (= try null)
            top
            try))))]
    (rec (.root @addr-tree))))

(defn- init
  []
  (update-mem!
    (vec (replicate mem-size null))))

(defn length-impl
  [v]
  (- (peek 1)))

(extend vector
  variable-size-memory-manager
  {:vlength length-impl})

(init)

(def x (atom 0))
(def r (Random.))
(dosync (ref-set mem (vec
  (map (fn [a]
    (swap! x (fn [b] (.nextInt r mem-size))) @x) @mem))))