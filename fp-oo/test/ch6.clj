(ns ch6
  (:require [clojure.test :refer :all]))

; The factorial function is a classic example of recursion. To recap: factorial of 0 is 1, factorial of 1
; is 1, factorial 2 is 2*1, factorial of 3 is 3*2*1, factorial of 4 is 4*3*2*1³.
; Factorial can fit our first recursive pattern, where the sequence of descending numbers is the
; structure to make smaller.
; Here’s that pattern. Write a factorial that follows it:

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

; Book solution
;(def factorial
;  (fn [n]
;    (if (or (= n 0)
;            (= n 1))
;      1
;      (* n (factorial (dec n))))))

(deftest factorial-test
  (are [n expected]
    (= (factorial n) expected)
    0 1
    1 (* 1)
    2 (* 2 1)
    3 (* 3 2 1)
    4 (* 4 3 2 1)
    ))

; Here is the second pattern:

(defn factorial2 [n]
  (loop [n n
         result 1]
    (if (>= 1 n)
      result
      (recur (dec n) (* n result)))))

; Book solution
;(def factorial-1
;  (fn [n so-far]
;    (if (or (= n 0)
;            (= n 1))
;      so-far
;      (factorial-1 (dec n) (* n so-far)))))
;
;(def factorial
;  (fn [n] (factorial-1 n 1)))

(deftest factorial2-test
  (are [n expected]
    (= (factorial2 n) expected)
    0 1
    1 (* 1)
    2 (* 2 1)
    3 (* 3 2 1)
    4 (* 4 3 2 1)
    ))

; Use the second pattern to make a recursive-function that can add a sequence of numbers.

(defn apply-seq [fun seq base]
  (loop [seq seq
         result base]
    (if (empty? seq)
      result
      (recur (rest seq) (fun (first seq) result)))))

; Book solution
;(def recursive-function
;  (fn [combiner something so-far]
;    (if (empty? something)
;      so-far
;      (recursive-function combiner
;                          (rest something)
;                          (combiner (first something)
;                                    so-far)))))

(deftest sum-seq-test
  (are [seq expected]
    (= (apply-seq + seq 0) expected)
    [] 0
    [1] 1
    [1 2] 3
    [1 2 3] 6
    [1 2 3 4] 10
    '(1 2 3 4) 10
    ))

; Now change the previous exercise’s function so that it can multiply a list of numbers.

(deftest mul-seq-test
  (are [seq expected]
    (= (apply-seq * seq 1) expected)
    [] 1
    [1] 1
    [1 2] 2
    [1 2 3] 6
    [1 2 3 4] 24
    '(1 2 3 4) 24
    ))

; Without changing apply-seq, choose starting values for the two wildcard parameters
; below that will cause it to convert a sequence of keywords into this rather silly map:
; user> (apply-seq **combiner**
;                  [:a :b :c]
;                  **starting-so-far**)
; {:a 0, :b 0, :c 0}
;
; A bit trickier is producing a map that associates each keyword with its position in the list:
; user> (apply-seq **combiner**
;                  [:a :b :c]
;                  **starting-so-far**)
; {:a 0, :b 1, :c 2}

(deftest silly-seq-test
  (is (= (apply-seq #(conj %2 {%1 0}) [:a :b :c] {}) {:a 0, :b 0, :c 0}))
  (is (= (apply-seq #(conj %2 {%1 (count %2)}) [:a :b :c] {}) {:a 0, :b 1, :c 2}))
  ; Book solution
  ;(is (= (apply-seq (fn [elt so-far] (assoc so-far elt 0)) [:a :b :c] {}) {:a 0, :b 0, :c 0}))
  ;(is (= (apply-seq (fn [elt so-far] (assoc so-far elt (count so-far))) [:a :b :c] {}) {:a 0, :b 1, :c 2}))
  )

; Pat yourself on the back! You have both used and implemented the built-in function reduce, perhaps
; the most dreaded of all sequence functions.
; reduce has a different order of arguments, but it does the same thing!

