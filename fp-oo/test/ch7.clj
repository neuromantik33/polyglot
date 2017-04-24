(ns ch7
  (:require [clojure.test :refer :all]))

; Use -> to process [1] by removing the number from the vector, incrementing it, and wrapping it in
; a list. The sequence of values would be this:
; [1]
; 1
; 2
; (2)

(deftest arrow-test
  (is (= (-> [1] first inc list) '(2)))
  )

; Add a step to the previous example. After incrementing the value, multiply it by 3, for this sequence
; of values:
; [1]
; 1
; 2
; 6
; (6)

(deftest arrow2-test
  (is (= (-> [1] first inc (* 3) list) '(6)))
  )

; Here’s a function that doubles a number:
; (fn [n] (* 2 n))
; Use that function instead of (* 2) in this chain:
; (-> 3 (* 2) inc)

(deftest arrow3-test
  (is (= (-> 3 (#(* 2 %)) inc) 7))
  )

; Convert (+ (* (+ 1 2) 3) 4) into a three-stage computation using ->:
; 1. The first step calculates (+ 1 2)
; 2. The result is passed to a step that multiplies the result by 3.
; 3. And that result is passed to a step that adds 4.

(deftest arrow4-test
  (is (= (-> (+ 1 2) (* 3) (+ 4)) 13))
  )
