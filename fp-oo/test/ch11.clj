(ns ch11
  (:require [clojure.test :refer :all])
  (:require [clojure.zip :as zip]))

;;; This is a handy function for inserting into a flow:
;;; (-> zipper zlog zip/right zlog...)
(def zlog
  (fn [z] (println "LOG:" (pr-str (zip/node z))) z))

;;; This prints the tree above the current node.
(def zuplog
  (fn [z] (zlog (zip/up z)) z))

; Write a function that collects all the vectors in a tree:
; => (all-vectors '(fn [a b] (concat [a] [b])))
; ([a b] [a] [b])

(defn all-vectors [tree]
  (letfn [(extract-v [vectors z]
            (let [val (zip/node z)
                  next (zip/next z)]
              (cond
                (zip/end? z) vectors
                (vector? val) (extract-v (cons val vectors) next)
                :else (extract-v vectors next))))]
    (reverse (extract-v '() (zip/seq-zip tree)))))

; Book solution
;(def all-vectors
;  (fn [tree]
;    (letfn [(all-from-zipper [so-far zipper]
;              (println (zip/node zipper))
;              (cond (zip/end? zipper)
;                    so-far
;
;                    (zip/branch? zipper)
;                    (all-from-zipper so-far (zip/next zipper))
;
;                    (vector? (zip/node zipper))
;                    (all-from-zipper (cons (zip/node zipper) so-far)
;                                     (zip/next zipper))
;
;                    :else
;                    (all-from-zipper so-far (zip/next zipper))))]
;      (reverse (all-from-zipper '() (zip/seq-zip tree))))))

(deftest all-vectors-test
  (is (= (all-vectors '()) '()))
  (is (= (all-vectors '([1] [2] '(3))) '([1] [2])))
  (is (= (all-vectors [[:thing]]) '([[:thing]])))
  (is (= (all-vectors '(fn [a b] (concat [a] [b]))) '([a b] [a] [b])))
  )

; Write a function that returns only the first vector in a tree. It should return nil if there is no vector.
; The easy way to do that would be this:
; (first (all-vectors ...))
; Don’t do it that way.

(defn first-vector [tree]
  (letfn [(first-v [z]
            (cond
              (zip/end? z) nil
              (vector? (zip/node z)) (zip/node z)
              :else (first-v (zip/next z))))]
    (first-v (zip/seq-zip tree))))

; Book solution
;(def first-vector
;  (fn [tree]
;    (letfn [(all-from-zipper [zipper]
;              (cond (zip/end? zipper)
;                    nil
;
;                    (vector? (zip/node zipper))
;                    (zip/node zipper)
;
;                    :else
;                    (all-from-zipper (zip/next zipper))))]
;      (all-from-zipper (zip/seq-zip tree)))))

(deftest first-vector-test
  (is (nil? (first-vector '())))
  (is (= (first-vector '([1] [2] '(3))) [1]))
  (is (= (first-vector [[:thing]]) [[:thing]]))
  (is (= (first-vector '(fn [a b] (concat [a] [b]))) '[a b]))
  (is (= (first-vector '(+ 1 (* 3 4))) nil))
  )

; If you’re like me, you understand code better after editing it. There’s a lot of duplication in tumult.
; Factor it out into helper functions.

(defn tumult [form]
  (letfn [(has-child? [z child]
            (and (zip/branch? z) (= (-> z zip/down zip/node) child)))
          (node-eq? [z val]
            (= (zip/node z) val))
          (do-and-proceed [z f]
            (-> z (f) zip/next helper))
          (helper [zipper]
            (cond
              (zip/end? zipper) zipper

              (node-eq? zipper '+)
              (do-and-proceed zipper #(zip/replace % 'PLUS))

              (has-child? zipper '-)
              (do-and-proceed zipper #(zip/append-child % 55555))

              (has-child? zipper '*)
              (do-and-proceed zipper #(zip/replace % '(/ 1 (+ 3 (- 0 9999)))))

              (node-eq? zipper '/)
              (do-and-proceed zipper #(-> %
                                          zip/right
                                          zip/remove
                                          zip/right
                                          zip/remove
                                          (zip/insert-right (-> zipper zip/right zip/node))
                                          (zip/insert-right (-> zipper zip/right zip/right zip/node))))

              :else
              (do-and-proceed zipper identity)
              ))]
    (-> form zip/seq-zip helper zip/root)))

; Book solution
;(def at?
;  (fn [zipper subtree] (= (zip/node zipper) subtree)))
;
;(def above?
;  (fn [zipper subtree]
;    (and (zip/branch? zipper)
;         (at? (zip/down zipper) subtree))))
;
;(def tumult
;  (fn [form]
;    (letfn [(advancing [flow]
;              (-> (flow) zip/next do-node))
;            (do-node [zipper]
;              (cond (zip/end? zipper)
;                    zipper
;
;                    (at? zipper '+)
;                    (advancing (fn [] (zip/replace zipper 'PLUS)))
;
;                    (above? zipper '-)
;                    (advancing (fn [] (zip/append-child zipper 55555)))
;
;                    (above? zipper '*)
;                    (advancing (fn [] (zip/replace zipper
;                                                   '(/ 1 (+ 3 (- 0 9999))))))
;
;                    (at? zipper '/)
;                    (advancing (fn []
;                                 (-> zipper
;                                     zip/right
;                                     zip/remove
;                                     zip/right
;                                     zip/remove
;                                     (zip/insert-right (-> zipper zip/right zip/node))
;                                     (zip/insert-right (-> zipper zip/right zip/right zip/node))
;                                     zip/next
;                                     do-node)))
;
;                    :else
;                    (advancing (constantly zipper))))]
;      (-> form zip/seq-zip do-node zip/root))))

(deftest tumult-test
  (is (= (tumult '(- 3 (+ 6 (+ 3 4) (* 2 1) (/ 8 3))))
         '(- 3 (PLUS 6 (PLUS 3 4) (/ (PLUS 3 (- 0 9999 55555)) 1) (/ 3 8)) 55555)))
  )

; Change tumult so that it replaces forms beginning with * with forms beginning with -. For example,
; suppose (* 1 2) was converted into (- 1 2). Given the predefined transformations for -, what
; would you expect the behavior of the following to be?
; (tumult '(* 1 2))
; What is it actually? Can you change it to be as you expect?

(defn tumult2 [form]
  (letfn [(has-child? [z child]
            (and (zip/branch? z) (= (-> z zip/down zip/node) child)))
          (node-eq? [z val]
            (= (zip/node z) val))
          (do-and-proceed [z f]
            (-> z (f) zip/next helper))
          (helper [zipper]
            (cond
              (zip/end? zipper) zipper

              (node-eq? zipper '+)
              (do-and-proceed zipper #(zip/replace % 'PLUS))

              (has-child? zipper '-)
              (do-and-proceed zipper #(zip/append-child % 55555))

              (node-eq? zipper '*)
              (do-and-proceed zipper #(-> %
                                          (zip/replace '-)
                                          zip/up
                                          (zip/append-child 55555)))

              (node-eq? zipper '/)
              (do-and-proceed zipper #(-> %
                                          zip/right
                                          zip/remove
                                          zip/right
                                          zip/remove
                                          (zip/insert-right (-> zipper zip/right zip/node))
                                          (zip/insert-right (-> zipper zip/right zip/right zip/node))))

              :else
              (do-and-proceed zipper identity)
              ))]
    (-> form zip/seq-zip helper zip/root)))

; Book solution
; See https://github.com/marick/fp-oo/blob/master/solutions/zipper.clj

(deftest tumult2-test
  (is (= (tumult2 '(* 1 2)) '(- 1 2 55555)))
  )

; Write a function transform that converts facts into do forms that wrap expect calls.
; Note that the arrow forms can appear deeply nested in a fact. For example, they’re often inside let
; expressions. Here’s an example from Midje’s own test suite:
; (fact "Metaconstants print as their name"
;   (let [mc (Metaconstant. '...name... {})]
;     (str mc) => "...name..."
;     (pr-str mc) => "...name..."))
; The only exception is that arrows may not be nested inside of either left-hand or right-hand size of
; an arrow. That is, the following is illegal:
; (fact
;   (3 => odd?) => "what could possibly made sense here?")
; The implication of that is that once you’ve transformed an arrow expression, you need not revisit
; the transformed version.

(def transform identity)

;; TO BE IMPLEMENTED

(deftest transform-test
  (is (= (transform
           '(facts
              (+ 1 2) => 3
              3 => odd?))
         '(do
            (expect (+ 1 2) => 3)
            (expect 3 => odd?))))
  )
