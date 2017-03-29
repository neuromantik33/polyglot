(ns ch0
  (:require [clojure.test :refer :all]))

;Define a function second that returns the second element of a list
(defn my-second [sequence]
  (if (< (count sequence) 2)
    nil
    (nth sequence 1)))

(deftest my-second-test
  (testing "(my-second '())    " (is (nil? (my-second '()))))
  (testing "(my-second '(1))   " (is (nil? (my-second '(1)))))
  (testing "(my-second '(1 2)) " (is (= (my-second '(1 2)) 2)))
  (testing "(my-second [9 8 7])" (is (= (my-second [9 8 7]) 8)))
  )

; Implement add-squares.
(defn add-squares [& numbers]
  (let [square (fn [x] (* x x))]
    (apply + (map square numbers))))

(deftest add-squares-test
  (testing "(add-squares)       " (is (= (add-squares) 0)))
  (testing "(add-squares 1)     " (is (= (add-squares 1) 1)))
  (testing "(add-squares 1 2)   " (is (= (add-squares 1 2) 5)))
  (testing "(add-squares 1 2 3) " (is (= (add-squares 1 2 3) 14)))
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
    (if (nil? c0)
      true
      (if (not= c0 s0)
        false
        (prefix-of? (rest candidate) (rest sequence))))))

(deftest prefix-of-test
  (testing "(prefix-of [] [])            " (is (prefix-of? [] [])))
  (testing "(prefix-of [1 2] [1 2 3 4])  " (is (prefix-of? [1 2] [1 2 3 4])))
  (testing "(prefix-of '(2 3) [1 2 3 4]) " (is (not (prefix-of? '(2 3) [1 2 3 4]))))
  (testing "(prefix-of? '(1 2) [1 2 3 4])" (is (prefix-of? '(1 2) [1 2 3 4])))
  )

; Implement this function: (tails sequence):
; Returns a sequence of successively smaller subsequences of the argument.

; Convoluted recursive solution!! (ie. mine)
; (defn tails
;   ([sequence]
;    (if (empty? sequence) sequence (tails sequence '())))
;   ([sequence result]
;    (let [n (count result)]
;      (if (= (count sequence) n)
;        (reverse (conj result '()))
;        (let [item (drop n sequence)]
;          (tails sequence (conj result item)))))))

; Much more elegant (also mine :))
(defn tails [sequence]
  (let [len (count sequence)
        drop-seq (fn [i] (drop i sequence))]
    (map drop-seq (range (inc len)))))

(deftest tails-test
  (testing "(tails '())       " (is (= (tails '()) '(()))))
  (testing "(tails '(1 2 3 4))" (is (= (tails '(1 2 3 4)) '((1 2 3 4) (2 3 4) (3 4) (4) ()))))
  )
