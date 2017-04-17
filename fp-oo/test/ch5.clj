(ns ch5
  (:require [clojure.test :refer :all]))

(defn- get-method-or-prop [name class]
  (let [method (name (:__instance_methods__ class))]
    (or method name)))

(defn apply-message-to [class instance message args]
  (let [m (get-method-or-prop message class)]
    (apply m instance args)))

(defn make [class & args]
  (let [instance {:__class_symbol__ (:__own_symbol__ class)}]
    (apply-message-to class instance :add-instance-values args)))

(defn send-to [instance message & args]
  (let [class (eval (:__class_symbol__ instance))]
    (apply-message-to class instance message args)))

(def Point
  {
   :__own_symbol__ 'Point
   :__instance_methods__
                   {
                    :add-instance-values (fn [this x y]
                                           (assoc this :x x :y y))
                    :class               (fn [instance]
                                           (eval (:__class_symbol__ instance)))
                    :class-name          :__class_symbol__
                    :shift               (fn [this xinc yinc]
                                           (make Point
                                                 (+ (:x this) xinc)
                                                 (+ (:y this) yinc)))
                    :add                 (fn [this other]
                                           (send-to this :shift
                                                    (:x other) (:y other)))
                    :origin              (fn [this] (make Point 0 0))
                    }
   })

; The last two steps of make and send-to are very similar. Both look up an instance
; method in a class, then apply that method to an object and arguments. Extract a common function
; apply-message-to that takes a class, an instance, a message, and a sequence of arguments. Like this:
; (def apply-message-to
;     (fn [class instance message args]
;          ...))
;
; (apply-message-to Point a-point :shift [1 3])
; It should use the class (a map, not a symbol) and the message (a keyword) to find a method/function.
; It should apply that method to the instance and the args. (You may have noticed that you can get
; the class from the instance, so those two separate parameters are not strictly needed. I thought it
; worthwhile to give each of the key values names in the parameter list.)
; Next, use apply-message-to within both make and send-to.

(def point (make Point 1 2))

(deftest apply-message-to-test
  (is (= (apply-message-to Point point :shift [-1 -2]) {:__class_symbol__ 'Point :x 0 :y 0}))
  (is (= (make Point 1 0) {:__class_symbol__ 'Point :x 1 :y 0}))
  (is (= (send-to point :shift -1 -2) {:__class_symbol__ 'Point :x 0 :y 0}))
  )

; Up until now, the :class message has returned the symbol naming the class. That was
; OK while an object’s class was nothing but a symbol. But now it’d be more appropriate to have
; class-name return the symbol and class return the actual class map.

(deftest class-test
  (is (= (send-to point :class-name) 'Point))
  (is (= (send-to point :class) Point))
  )

; Some languages (or development environments) make it easy to define accessor (getter or setter)
; methods at the same time you define instance variables. Let’s do something like that.
; Change apply-message-to so that if it finds there is no matching method, it looks to see if there’s a
; matching instance variable and (if so) returns its value.

(def Holder
  {
   :__own_symbol__ 'Holder
   :__instance_methods__
                   {
                    :add-instance-values (fn [this held]
                                           (assoc this :held held))
                    }
   })

(deftest holder-test
  (is (= (send-to (make Holder "stuff") :held) "stuff"))
  )
