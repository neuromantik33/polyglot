(ns ch4
  (:require [clojure.test :refer :all]))

(def make
  (fn [type & args]
    (apply type args)))

(def send-to
  (fn [object message & args]
    (apply (message (:__methods__ object)) object args)))

(def Point
  (fn [x y]
    {
     :x                x,
     :y                y
     :__class_symbol__ 'Point
     :__methods__      {
                        :class :__class_symbol__
                        :x     :x
                        :y     :y
                        :shift (fn [this xinc yinc]
                                 (let [x (+ (send-to this :x) xinc)
                                       y (+ (send-to this :y) yinc)]
                                   (make Point x y)))
                        :add   (fn [this other]
                                 (send-to this :shift (send-to other :x) (send-to other :y)))
                        }
     }
    ))

; Change the Point constructor to add x and y accessors (getters).
; Use them in shift. Implement add, and have it use shift.
(def point (make Point 1 2))

(deftest getter-test
  (is (= (send-to point :x) 1))
  (is (= (send-to point :y) 2))
  )

(deftest shift-test
  (is (= (select-keys (send-to point :shift -1 -2) [:x :y]) {:x 0 :y 0}))
  )

(deftest add-test
  (is (= (select-keys (send-to point :add (make Point 1 2)) [:x :y]) {:x 2 :y 4}))
  )


