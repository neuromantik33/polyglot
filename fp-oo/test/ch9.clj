(ns ch9
  (:require [clojure.test :refer :all]))

; In a free country, you could add 2 to each element of a sequence like this:
; (map (fn [n] (+ 2 n)) [1 2 3])
; However, ever since the Point-Free Programmer’s Brigade seized control of the government, fn has
; been banned. How else could you accomplish your goal? Can you think of more than one way?

(def inc-by-2 (partial + 2))

; Book solution
; (def inc-by-2 (comp inc inc))

(deftest add-2-test
  (is (= (map (fn [n] (+ 2 n)) [1 2 3])
         (map inc-by-2 [1 2 3])
         [3 4 5]))
  )

; juxt turns n functions into a single function that returns a vector whose first element comes from
; the first function, the second from the second function, and so on.
; => ( (juxt empty? reverse count) [:a :b :c])
; [false (:c :b :a) 3]
; Define it using juxt.

(defn my-juxt [& functions]
  (fn [v]
    (map #(% v) functions)))

(deftest juxt-test
  (is (= ((my-juxt empty? reverse count) [:a :b :c]) [false '(:c :b :a) 3]))
  )

; If let didn’t exist, could you use functions to achieve the same effect as :
; (def always-3
;   (let [x 3]
;     (fn [] x)))
; That is, what code could you wrap around (fn [] x) to produce an x that was bound to no value
; outside the function and bound to 3 inside it?

(def always-3 (constantly 3))

; Book solution
;(def always-3
;  ((fn [x]
;     (fn [] x))
;    3))

(deftest always-3-test
  (is (= (always-3) 3))
  )

; Clojure has datatypes that support mutability in the presence of concurrency. For example, an atom
; is an object protected from overlapping updates by multiple threads. You update an atom by applying
; a function to its current value:
; => (def my-atom (atom 0))
; => (swap! my-atom inc)
; => (deref my-atom)
; 1
; Suppose you wanted to set an atom’s value to 33, regardless of its current value. How would you do
; that? Keep in mind that swap! demands a function, not (say) an integer.

; Book solution
;(def my-atom (atom 0))
;(swap! my-atom (fn [anything] 33))

(deftest atom-test
  (let [my-atom (atom 0)
        _ (swap! my-atom (constantly 53))
        value (deref my-atom)]
    (is (= value 53)))
  )

; In the previous exercise, you probably hand-crafted (with fn) a function that returned a constant
; value. That meant you wrote a function with a parameter that was ignored in favor of the constant.
; If anything calls for point-free style, this is it.
; Write a function always that takes a value and produces a function that returns that value no matter
; what its arguments are. That is:
; => ( (always 8) 1 'a :foo)
; 8
; Compare your solution to Clojure’s constantly.

; NES Same exactly as constantly!!
(defn always [val]
  (fn [& args] val))

(deftest always-test
  (is (= ((always 666)) 666))
  (is (= ((always 8) 1 'a :foo) 8))
  )

; To practice for this book, I wrote three earlier ones. Here are their ISBNs: 0131774115, 0977716614,
; and 1934356190 — except that one of them contains a typo. In the next two exercises, you’re to find
; which one.

; Common functions
(defn positive-integers [len]
  (-> len inc (#(range 1 %))))

(defn reversed-digits [string]
  (let [toInt #(Character/getNumericValue %)]
    (map toInt (reverse string))))

(defn number-checker [checksum-fn div]
  (fn [string]
    (let [checksum (-> string reversed-digits checksum-fn (rem div))]
      (zero? checksum))))

; First, use map to write a function check-sum that performs the following calculation on the sequence
; [4 8 9 3 2]:
; (+ (* 1 4)
;    (* 2 8)
;    (* 3 9)
;    (* 4 3)
;    (* 5 2))
; check-sum should take any number of digits.

(defn isbn-checksum [ints]
  (let [factors (positive-integers (count ints))
        products (map * factors ints)]
    (apply + products)))

; Book solution
;(def isbn-checksum
;  (fn [sequence]
;    (apply + (map *
;                  (range 1 (inc (count sequence)))
;                  sequence))))

(deftest isbn-checksum-test
  (is (= (isbn-checksum [4 8 9 3 2]) 69))
  )

; A valid ISBN has a check sum that can be divided by 11 to leave a remainder of 0. Write a function
; isbn? that checks if a string is a valid ISBN. You can use (rem N 11) to find the remainder.

(def isbn? (number-checker isbn-checksum 11))

(deftest isbn?-test
  (is (isbn? "0131774115"))
  (is (isbn? "1934356190"))
  (is (not (isbn? "0977716614")))
  )

; Universal Product Codes (UPCs) need a slightly more complicated check-sum. Here’s an example of
; the calculation for [4 8 9 3 2]:
;  (+ (* 1 4)
;     (* 3 8)
;     (* 1 9)
;     (* 3 3)
;     (* 1 2))
; If the position is odd, the number is multiplied by 1, otherwise 3. The divisor is 10, not 11.
; Implement check-sum and upc?

(defn upc-checksum [ints]
  (let [multiplier #(if (odd? %) 1 3)
        factors (map multiplier (positive-integers (count ints)))
        products (map * factors ints)]
    (apply + products)))

; Book solution
;(def upc-checksum
;  (fn [sequence]
;    (apply + (map (fn [position digit]
;                    (* digit (if (odd? position) 1 3)))
;                  (range 1 (inc (count sequence)))
;                  sequence))))

(deftest upc-checksum-test
  (is (= (upc-checksum [4 8 9 3 2]) 48))
  )

(def upc? (number-checker upc-checksum 10))

(deftest upc?-test
  (is (upc? "074182265830"))
  (is (upc? "731124100023"))
  (is (not (upc? "722252601404")))
  )

; The same general “shape” of function will work for checking credit cards, money orders, and so
; on. Extract the commonality of isbn? and upc? (and their respective check-sums) into a function
; number-checker that can be used to create either of them. Like this:
; (def isbn? (number-checker ...))
; (def upc? (number-checker ...))

; Book solution
;(def number-checker
;  (fn [digit-function divisor]
;    (fn [candidate]
;      (let [digits (reversed-digits candidate)
;            check-sum (apply +
;                             (map digit-function
;                                  (range 1 (inc (count digits)))
;                                  digits))]
;        (zero? (rem check-sum divisor))))))
;
;(def isbn? (number-checker * 11))
;(def upc? (number-checker
;            (fn [position digit] (* digit (if (odd? position) 1 3)))
;            10))
