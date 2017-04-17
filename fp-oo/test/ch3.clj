(ns ch3
  (:require [clojure.test :refer :all]))

(def Point
  (fn [x y]
    {:x                x,
     :y                y
     :__class_symbol__ 'Point}))

(def x :x)
(def y :y)

(def shift
  (fn [this xinc yinc]
    (Point (+ (x this) xinc)
           (+ (y this) yinc))))


(def Triangle
  (fn [point1 point2 point3]
    {:point1           point1,
     :point2           point2,
     :point3           point3
     :__class_symbol__ 'Triangle}))

(def right-triangle
  (Triangle (Point 0 0) (Point 0 1) (Point 1 0)))

(def equal-right-triangle
  (Triangle (Point 0 0) (Point 0 1) (Point 1 0)))

(def different-triangle
  (Triangle (Point 0 0) (Point 0 10) (Point 10 0)))

; Implement an add function that adds two points, producing a third. First implement it without using
; shift. Then implement it using shift. (If you think of add as an instance method, calling shift is
; like using another instance method in the same class.)
(defn add-1 [p1 p2]
  (Point
    (+ (x p1) (x p2))
    (+ (y p1) (y p2))))

(defn add-2 [p1 p2]
  (shift p1 (x p2) (y p2)))

(deftest add-test
  (is (= (add-1 (Point 1 0) (Point 0 1))
         (add-2 (Point 1 0) (Point 0 1))
         (Point 1 1)))
  )

; Our Point function matches the way Python constructors are called. Java would use new Point and
; Ruby would use Point.new. That suggests an alternative syntax:
; (new Point 3 5)
; However, Clojure reserves new for the creation of Java objects, so we’ll use make instead:
; (make Point 1 2)
; Write the function make, assuming that the function Point already exists.
(defn make [ctr & args]
  (apply ctr args))

; Book solution
;(def make
;  (fn [type arg1 arg2]
;    (type arg1 arg2)))

(deftest make-test
  (testing "Making points   " (is (= (make Point 1 2) (Point 1 2))))
  (testing "Making triangles" (is (= (make Triangle (Point 0 0) (Point 0 1) (Point 1 0)) right-triangle)))
  )

; Write a function equal-triangles?
(def equal-triangles? =)

(deftest equal-triangles?-test
  (testing "Identical triangles     " (is (equal-triangles? right-triangle right-triangle)))
  (testing "Not identical, but equal" (is (equal-triangles? right-triangle equal-right-triangle)))
  (testing "Different               " (is (not (equal-triangles? right-triangle different-triangle))))
  (testing "Different (3 args)" (is (not (equal-triangles? right-triangle equal-right-triangle different-triangle))))
  )

; Write a function valid-triangle? that takes three Points and returns either true or false.
(defn valid-triangle? [p1 p2 p3]
  (distinct? p1 p2 p3))

; Book solution
;(def valid-triangle?
;  (fn [& points]
;    (= (distinct points) points)))

(deftest equal-triangles?-test
  (testing "Valid triangle             " (is (valid-triangle? (Point 0 0) (Point 0 1) (Point 1 0))))
  (testing "1st and 2nd point are equal" (is (not (valid-triangle? (Point 0 0) (Point 0 0) (Point 1 1)))))
  (testing "1st and 3rd point are equal" (is (not (valid-triangle? (Point 1 1) (Point 0 0) (Point 1 1)))))
  (testing "2nd and 3nd point are equal" (is (not (valid-triangle? (Point 0 0) (Point 1 1) (Point 1 1)))))
  )
