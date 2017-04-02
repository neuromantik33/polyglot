(ns ch0
  (:require [clojure.test :refer :all]))

; Define a function second that returns the second element of a list
(defn my-second [sequence]
  (if (< (count sequence) 2) nil (nth sequence 1)))

(deftest my-second-test
  (testing "(my-second '())    " (is (nil? (my-second '()))))
  (testing "(my-second '(1))   " (is (nil? (my-second '(1)))))
  (testing "(my-second '(1 2)) " (is (= (my-second '(1 2)) 2)))
  (testing "(my-second [9 8 7])" (is (= (my-second [9 8 7]) 8)))
  )

; Give 2 implementations of third
(defn third-1 [sequence]
  (if (< (count sequence) 3) nil (nth sequence 2)))
(defn third-2 [sequence]
  (second (rest sequence)))

(deftest third-test
  (testing "(third-1 [0 1 2 3 4])" (is (= (third-1 [0 1 2 3 4]) (third-2 [0 1 2 3 4]) 2)))
  )

; Implement add-squares.
(defn add-squares [& numbers]
  (let [square #(* % %)]
    (apply + (map square numbers))))

; Book solution
;(def add-squares
;  (fn [& numbers]
;    (apply + (map * numbers numbers))))

(deftest add-squares-test
  (testing "(add-squares)      " (is (= (add-squares) 0)))
  (testing "(add-squares 1)    " (is (= (add-squares 1) 1)))
  (testing "(add-squares 1 2)  " (is (= (add-squares 1 2) 5)))
  (testing "(add-squares 1 2 3)" (is (= (add-squares 1 2 3) 14)))
  )

; Using range and apply, implement a bizarre version of factorial
; that uses neither iteration nor recursion.
(defn fact [n]
  (if (zero? n)
    1
    (apply * (range 1 (inc n)))))

(deftest fact-test
  (testing "(fact 0)" (is (= (fact 0) 1)))
  (testing "(fact 1)" (is (= (fact 1) 1)))
  (testing "(fact 2)" (is (= (fact 2) 2)))
  (testing "(fact 5)" (is (= (fact 5) 120)))
  )

; Implement this function: (prefix-of? candidate sequence):
; Both arguments are sequences. Returns true if the elements
; in the candidate are the first elements in the sequence:.
(defn prefix-of? [candidate sequence]
  (let [c0 (first candidate)
        s0 (first sequence)]
    (cond
      (nil? c0) true
      (not= c0 s0) false
      :else (recur (rest candidate) (rest sequence)))))

; Book solution
;(def prefix-of?
;  (fn [candidate seq]
;    (= (take (count candidate) seq)
;       candidate)))

(deftest prefix-of-test
  (testing "(prefix-of [] [])            " (is (prefix-of? [] [])))
  (testing "(prefix-of [1 2] [1 2 3 4])  " (is (prefix-of? [1 2] [1 2 3 4])))
  (testing "(prefix-of '(2 3) [1 2 3 4]) " (is (not (prefix-of? '(2 3) [1 2 3 4]))))
  (testing "(prefix-of? '(1 2) [1 2 3 4])" (is (prefix-of? '(1 2) [1 2 3 4])))
  )

; Implement this function: (tails sequence):
; Returns a sequence of successively smaller subsequences of the argument.

; Convoluted recursive solution!! (ie. mine)
;(defn tails [sequence]
;  (loop [seq sequence
;         result '()]
;    (let [n (count result)]
;      (if (< (count seq) n)
;        (reverse result)
;        (let [item (drop n seq)]
;          (recur seq (conj result item)))))))

; Much more elegant (also mine :))
(defn tails [sequence]
  (let [len (count sequence)
        drop-seq #(drop % sequence)]
    (map drop-seq (range (inc len)))))

; Book solution
;(def tails
;  (fn [seq]
;    (map drop
;         (range (inc (count seq)))
;         (repeat (inc (count seq)) seq))))

(deftest tails-test
  (testing "(tails '())       " (is (= (tails '()) '(()))))
  (testing "(tails '(1 2 3 4))" (is (= (tails '(1 2 3 4)) '((1 2 3 4) (2 3 4) (3 4) (4) ()))))
  )
