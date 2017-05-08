(ns ch10
  (:require [clojure.test :refer :all])
  (:require [clojure.algo.monads :refer :all]))

; Convert this into continuation-passing style:
;(let [a (concat '(a b c) '(d e f))
;      b (count a)]
;  (odd? b))

(-> (concat '(a b c) '(d e f))
    ((fn [a]
       (-> (count a)
           ((fn [b]
              (odd? b)))))))

; This code computes the same value as the previous code. Rewrite it into continuation-passing style.
; (odd? (count (concat '(a b c) '(d e f))))

(-> '(a b c)
    ((fn [a]
       (-> (concat a '(d e f))
           ((fn [b]
              (-> (count b)
                  ((fn [c]
                     (odd? c))))))))))

; Convert this into continuation-passing style:
;(-> 3
;    (+ 2)
;    inc)

(-> 3
    ((fn [a]
       (-> (+ a 2)
           ((fn [b]
              (inc b)))))))

; Error utilities
(defn oops! [reason & args]
  (with-meta (merge {:reason reason} (apply hash-map args))
             {:type :error}))

(defn oopsie? [value]
  (= (type value) :error))

; Often, receiving a nil from the Maybe monad is not so useful. Something bad happened somewhere,
; but you don’t know where or what. Furthermore, not every error is associated with a nil. In this
; exercise, you’ll implement a monad that has the short-circuiting behavior of the Maybe monad, but
; works with error values instead of nils.
; For example, suppose we have this function:

(defn factorial [n]
  (cond (< n 0) (oops! "Factorial can never be less than zero." :number n)
        (< n 2) 1
        :else (* n (factorial (dec n)))))

; Your job in this exercise is to write a monad that halts computation when an oopsie? is produced
; by a step.

(def error-monad
  (let [bind (fn [val continue]
               (if (oopsie? val) val (continue val)))]
    (monad [m-result identity
            m-bind bind])))

(defn fact-sqr-3-times [n]
  (with-monad error-monad
              (domonad [big-number (factorial n)
                        even-bigger (* 2 big-number)]
                       (repeat 3 even-bigger))))

(deftest error-monad-test
  (is (= (fact-sqr-3-times 3) [12 12 12]))
  (let [error (fact-sqr-3-times -1)]
    (is (oopsie? error))
    (is (= (:reason error) "Factorial can never be less than zero."))
    (is (= (:number error) -1))
    ))

; Write a function multiples that takes a number and returns a sequence of all its non-prime multiples
; less than 100.

(defn multiples [n]
  (range (* n 2) 101 n))

(deftest multiples-test
  (is (= (multiples 2) (range (* 2 2) 101 2)))
  (is (= (multiples 3) (range (* 3 2) 101 3)))
  (is (= (multiples 4) (range (* 4 2) 101 4)))
  )

; Use the Sequence monad or for (your choice!) to find all non-primes less than 100. Duplicates are OK.

(require '(clojure [set :as s]))
(def primes #{2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97})
(def non-primes (s/difference (set (range 1 100)) primes))

(defn not-prime? [n]
  (if (= n 1)
    true
    (some? (some zero? (map #(rem n %) (range 2 n))))))

(defn gen-non-primes [n]
  (for [x (range 1 100)
        :when (not-prime? x)]
    x))

; Book solution
;(def nonprimes
;  (for [i (range 2 11) ; You only need to try up to the square root of 100.
;        nonprime (multiples i)]
;    nonprime))
;
; Seq monad version
;(def nonprimes
;  (with-monad sequence-m
;              (domonad [i (range 2 11) ; You only need to try up to the square root of 100.
;                        nonprime (multiples i)]
;                       nonprime)))

(deftest gen-non-primes-test
  (is (= (gen-non-primes 100) (sort non-primes)))
  )

; Use sets to calculate all the primes less than 100.

(defn gen-primes [n]
  (s/difference (set (range 1 n)) (gen-non-primes n)))

; Book solution
;(def primes
;  (remove (set nonprimes) (range 2 101)))

(deftest gen-primes-test
  (is (= (gen-primes 100) primes))
  )
