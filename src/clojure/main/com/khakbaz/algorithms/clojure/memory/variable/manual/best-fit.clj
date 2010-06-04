(ns com.khakbaz.algorithms.clojure.memory.variable.manual.best-fit
  (:import
    [java.lang
     IndexOutOfBoundsException
     IllegalArgumentException])
  (:use [com.khakbaz.algorithms.clojure.memory.variable.vector
         :only [null variable-size-memory-manager
                variable-size-manual-memory-manager]])
  (:refer-clojure :rename
    {peek clojure-peek vector clojure-vector}))

(deftype vector
  [addr])

(def
  #^{:private true}
  min-size 5)

(def
  #^{:private true}
  mem-size 100)

(defprotocol binary-tree-comparison
  (<?
    [self node])
  (>?
    [self node])
  (=?
    [self node]))

(defprotocol binary-tree-navigation
  (left-child
    [self node]
    [self])
  (left-child!
    [self node val]
    [self val])
  (right-child
    [self node]
    [self])
  (right-child!
    [self node val]
    [self val])
  (null?
    [self]))

(def
  #^{:private true}
  mem (ref nil))

(defn- peek
  [addr]
  (nth @mem addr))

(defn- poke
  [addr val]
  (assoc @mem addr val))

(defn- poke!
  [addr val]
  (dosync
    (ref-set mem (poke addr val))))

(deftype
  weird-tree
  [root offset])

(defn- weird-tree-operator-dispatcher
  [op]
  (fn [self operand]
    (op (.root self) operand)))

(extend weird-tree
  binary-tree-navigation
  {:left-child
  (fn
    ([self addr]
      (peek (+ (.offset self) addr)))
    ([self]
      (peek (+ (.offset self) (.root self)))))
   :left-child!
   (fn
     ([self addr val]
       (poke! (+ addr (.offset self)) val))
     ([self val]
       (poke! (+ (.root self) (.offset self)) val)))
   :right-child
   (fn
     ([self addr]
       (peek (+ addr (.offset self) 2)))
     ([self]
       (peek (+ (.root self) (.offset self) 2))))
   :right-child!
   (fn
     ([self addr val]
       (poke! (+ addr (.offset self) 2) val))
     ([self val]
       (poke! (+ (.root self) (.offset self) 2) val)))
   :null?
   (fn [self]
     (= (.root self) null))
   :peek
   (fn [self]
     (peek (.root self)))}
  binary-tree-comparison
   {:<?
   (weird-tree-operator-dispatcher <)
   :=?
   (weird-tree-operator-dispatcher =)
   :>?
   (weird-tree-operator-dispatcher >)})

(def
  #^{:private true}
  size-tree-offset
  1)

(def
  #^{:prviate true}
  addr-tree-offset
  3)

(defn- make-size-tree
  [root]
  (weird-tree.
    root size-tree-offset))

(defn- make-addr-tree
  [root]
  (weird-tree.
    root addr-tree-offset))

(def
  #^{:private true}
  size-tree
  (ref (make-size-tree null)))

(def
  #^{:private true}
  addr-tree
  (ref (make-addr-tree null)))

(defn- update-mem!
  [new-mem]
  (dosync
    (ref-set mem new-mem)))

(defn- size
  [addr]
  (peek addr))

(defn- size!
  [addr new-size]
  (poke! addr new-size))

(defn- set-addr-tree!
  [addr]
  (dosync
    (ref-set addr-tree
      (make-addr-tree addr))))

(defn- set-size-tree!
  [siz]
  (println "setting the size-tree to " siz)
  (dosync
    (ref-set size-tree
      (make-size-tree siz))))

(defn- set-size-and-addr-trees!
  [siz addr]
  (dosync
    (ref-set size-tree
      (make-size-tree siz))
    (ref-set addr-tree
      (make-addr-tree addr))))

(defn- addr-tree-root
  []
  (.root @addr-tree))

(defn- size-tree-root
  []
  (.root @size-tree))

(defn- find-size-helper
  [siz top]
  (println "start" siz top)
  (cond
    (= top null)
    (do
      (println "branch-1" top siz null)
      null)
    (= (size top) siz)
    (do
      (println "branch-2")
      top)
    (< (size top) siz)
    (do
      (println "branch-3")
      (find-size-helper siz
        (right-child @size-tree top)))
    :else
    (do (println "branch-4")
      (let [try (find-size-helper siz (left-child @size-tree top))]
        (println "try = " try)
        (if (= try null)
          top
          try)))))

(defn- find-size
  [siz]
  (find-size-helper siz (size-tree-root)))

(defn- find-addr-helper
  [addr top]
  (cond
    (null? top) null
    (=? top addr)
    (.root top)
    (>? top addr)
    (find-addr-helper
      (left-child top))
    :else
    (let [try (find-addr-helper (right-child top))]
      (if (= try null)
        (.root top)
        try))))

(defn- find-addr
  [addr]
  (find-addr-helper addr @addr-tree))

(defn- insert-address
  [addr siz top]
  (if (<? top addr)
    (let [right (make-addr-tree (right-child top))]
      (if (null? right)
        (right-child! top addr)
        (insert-address right)))
    (let [left (make-addr-tree (left-child top))]
      (if (null? left)
        (left-child! top addr)
        (insert-address left)))))

(defn- insert-size
  [addr siz top]
  (if (>= (size top) siz)
    (let [left (left-child @size-tree top)]
      (if (= left null)
        (left-child! @size-tree top addr)
        (insert-size left)))
    (let [right (right-child @size-tree top)]
      (if (= right null)
        (right-child! @size-tree top addr)
        (insert-size right)))))

(defn- insert-free
  [addr siz]
  (println "start insert free" addr siz)
  (dosync
    (size! addr siz)
    (left-child! @addr-tree addr null)
    (right-child! @addr-tree addr null)
    (left-child! @size-tree addr null)
    (right-child! @size-tree addr null))
  (println "before insert-address")
  (if (null? @addr-tree)
    (set-addr-tree! addr)
    (insert-address addr siz @addr-tree))
  (println "after insert-address before insert-size")
  (if (null? @size-tree)
    (set-size-tree! addr)
    (insert-size addr siz (size-tree-root))))

(defn- delete-next-address [addr node op]
  (cond
    (= (right-child node) null)
    [(op (left-child node))
     node]
    :else
    (delete-next-address addr
      (make-addr-tree
        (right-child node))
      (fn [ref]
        (right-child! node
          (make-addr-tree ref))))))

(defn- delete-address [addr node op]
  (cond
    (> addr (.root node))
    (delete-address addr
      (make-addr-tree
        (right-child node))
      (fn [ref]
        (right-child! node
          (make-addr-tree ref))))
    (= (left-child node) null)
    (op (right-child node))
    (= (right-child node) null)
    (op (left-child node))
    :else
    (let
      [hold
       (delete-next-address addr
         (make-addr-tree
           (left-child node))
         (fn [ref]
           (left-child! node
             (make-addr-tree ref))))]
      (left-child! hold (left-child node))
      (right-child hold (right-child node))
      (op hold))))

(defn- delete-next-size [addr node op]
  (cond
    (= (left-child node) null)
    (do
      (op (right-child node))
      node)
    :else
    (delete-next-size addr
      (make-size-tree
        (left-child node))
      (fn [ref]
        (left-child! node
          (make-size-tree ref))))))

(defn- delete-size [addr node op]
  (let [siz (size addr)]
    (cond
      (< siz (size (.root node)))
      (do
        (println "delete-size-branch-1:")
        (delete-size addr
          (make-size-tree
            (left-child node))
          (fn [ref]
            (left-child! node
              (make-addr-tree ref)))))
      (> siz (size (.root node)))
      (do
        (println "delete-size-branch-2:")
        (delete-size addr
          (make-size-tree
            (right-child node))
          (fn [ref]
            (right-child! node
              (make-size-tree ref)))))
      (not= addr (.root node))
      (do
        (println "delete-size-branch-3:" addr (.root node))
        (delete-size addr
          (make-size-tree
            (right-child node))
          (fn [ref]
            (right-child! node
              (make-size-tree ref)))))
      (= (left-child node) null)
      (do
        (println "delete-size-branch-4:")
        (op (right-child node)))
      (= (right-child node) null)
      (do
        (println "delete-size-branch-5:")
        (op (left-child node)))
      :else
      (do
        (println "delete-size-branch-6:")
        (let
          [hold
           (delete-next-size addr
             (make-size-tree
               (right-child node))
             (fn [ref]
               (right-child! node
                 (make-size-tree ref))))]
          (left-child! hold (left-child node))
          (right-child! hold (right-child node))
          (op hold))))))

(defn- delete-free [addr]
  (delete-address addr @addr-tree set-addr-tree!)
  (println "after delete-address and before delete-size")
  (delete-size addr @size-tree set-size-tree!))

(defn- reset-free
  []
  (set-size-and-addr-trees!
    null null))

(defn- init
  []
  (update-mem!
    (vec (replicate mem-size null)))
  (insert-free 0 mem-size))

(defn- address-plus-index
  [addr idx]
  (if (not (number? idx))
    (throw (IllegalArgumentException.
      "Index must be a number, given:" idx)))
  (if (< idx 0)
    (throw (IllegalArgumentException. "Index must be non-negative, given:" idx)))
  (let
    [size (- (peek addr) 1)]
    (if (>= idx size)
      (throw
        (IndexOutOfBoundsException.
          "This index is too high or too low:")))
    (+ addr idx 1)))

(defn- make-vector-helper
  [siz]
  (let
    [hold-addr (find-size siz)]
    (println "find-size done" hold-addr mem)
    (if (= hold-addr null)
      null
      (let [hold-size (peek hold-addr)]
        (println "befor delete-free" hold-addr)
        (delete-free hold-addr)
        (println "after delete-free" hold-size hold-addr)
        (if (>= (- hold-size siz) min-size)
          (do
            (println "before insert-free")
            (insert-free (+ hold-addr siz) (- hold-size siz))
            (poke! hold-addr siz))
          (poke! hold-addr hold-size))
        (vector. hold-addr)))))

(defn make-vector
  [size]
  (make-vector-helper
    (if (< (inc size) min-size)
      min-size
      (inc size))))

(defn vector-ref
  [v i]
  (peek (address-plus-index (.addr v) i)))

(defn vector-set!
  [v i val]
  (poke! (address-plus-index (.addr v) i) val))

(defn vector-length
  [v]
  (- (peek (.addr v)) 1))

;(defn vector-free
;  [v]
;  (let
;     [addr (.addr v)
;      siz (size addr)
;      hold-addr (make-addr-tree (find-addr (+ addr siz)))]
;   (if (not (null? hold-addr))
;    (let
;      [hold-size (peek hold-addr)]
;      (when (=? hold-addr (+ addr siz))
;        (delete-free (.root hold-addr))

(extend vector
  variable-size-memory-manager
  {:vector-ref
  vector-ref
   :vector-set!
   vector-set!
   :vector-length
   vector-length})

(init)
(def v (make-vector 10))