(ns com.khakbaz.algorithms.clojure.memory.variable.manual.best-fit
  (:import
    [java.util Random]
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

(defprotocol binary-tree-navigator
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

(extend weird-tree
  binary-tree-navigator
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
     (= (.root self) null))})

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

(defn- find-size
  [siz]
  (letfn
    [(rec [top]
      (cond
        (= top null)
        null
        (= (size top) siz)
        top
        (< (size top) siz)
        (rec (right-child @size-tree top))
        :else
        (let [try (rec (left-child @size-tree top))]
          (if (= try null)
            top
            try))))]
    (rec (size-tree-root))))

(defn- find-addr
  [addr]
  (letfn
    [(rec
      [top]
      (cond
        (= top null) null
        (= top addr) top
        (> top addr)
        (rec (left-child @addr-tree top))
        :else
        (let [try (rec (right-child @addr-tree top))]
          (if (= try null)
            top
            try))))]
    (rec (addr-tree-root))))

(defn- insert-free
  [addr siz]
  (letfn
    [(insert-address
      [top]
      (if (< top addr)
        (let [right (right-child @addr-tree top)]
          (if (= right null)
            (right-child! @addr-tree top addr)
            (insert-address right)))
        (let [left (left-child @addr-tree top)]
          (if (= left null)
            (left-child! @addr-tree top addr)
            (insert-address left)))))
     (insert-size
       [top]
       (if (>= (size top) siz)
         (let [left (left-child @size-tree top)]
           (if (= left null)
             (left-child! @size-tree top addr)
             (insert-size left)))
         (let [right (right-child @size-tree top)]
           (if (= right null)
             (right-child! @size-tree top addr)
             (insert-size right)))))]
    (dosync
      (size! addr siz)
      (left-child! @addr-tree addr null)
      (right-child! @addr-tree addr null)
      (left-child! @size-tree addr null)
      (right-child! @size-tree addr null))
    (if (null? @addr-tree)
      (set-addr-tree! addr)
      (insert-address (addr-tree-root)))
    (if (null? @size-tree)
      (set-size-tree! siz)
      (insert-address (size-tree-root)))))

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
    [(op (right-child node))
     node]
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
      (delete-size addr
        (make-size-tree
          (left-child node))
        (fn [ref]
          (left-child! node
            (make-addr-tree ref))))
      (> siz (size (.root node)))
      (delete-size addr
        (make-size-tree
          (right-child node))
        (fn [ref]
          (right-child! node
            (make-size-tree ref))))
      (not= addr node)
      (delete-size addr
        (make-size-tree
          (right-child node))
        (fn [ref]
          (right-child! node
            (make-size-tree ref))))
      (= (left-child node) null)
      (op (right-child node))
      (= (right-child node) null)
      (op (left-child node))
      :else
      (let
        [hold
         (delete-next-size addr
           (make-size-tree
             (right-child node))
           (fn [ref]
             (right-child! node
               (make-size-tree ref))))]))))

(defn- delete-free [addr]
  (delete-address addr @addr-tree set-addr-tree!)
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
    (throw (IllegalArgumentException. "Must be positive")))
  (let
    [size (- (peek addr) 1)]
    (if (>= idx size)
      (throw
        (IndexOutOfBoundsException.
          "Your index is too high or too low")))
    (+ addr idx 1)))

;(extend vector
;  variable-size-memory-manager
;  {:vlength length-impl})

(init)