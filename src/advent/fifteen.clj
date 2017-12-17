(ns advent.fifteen
  (:require [clojure.string :as str]))

(def div 2147483647)

(defn generator [starting-value factor]
  (fn []
    (let [f #(rem (* factor %) div)
          start (f (bigint starting-value))]
      (iterate f start))))

(def example-generator-a (generator 65 16807))
(def example-generator-b (generator 8921 48271))

(def real-generator-a (generator 277 16807))
(def real-generator-b (generator 349 48271))

(defn to-binary-string [i]
  (Integer/toBinaryString i))

(defn with-leading-zeroes [desired-length s]
  (let [format-string (str "%" desired-length "s")
        with-spaces (format format-string s)]
    (str/replace with-spaces #" " "0")))

(defn trailing [s n]
  (let [start (- (count s) n)]
    (subs s start)))

(defn counts? [a b]
  (let [max-length (max (count a) (count b))
        comparable-a (with-leading-zeroes max-length a)
        comparable-b (with-leading-zeroes max-length b)]
    (= (trailing comparable-a 16)
       (trailing comparable-b 16))))

(defn judge [a-generator b-generator n]
  (let [as (map to-binary-string (take n (a-generator)))
        bs (map to-binary-string (take n (b-generator)))]
    (count (filter (partial apply counts?) (partition 2 (interleave as bs))))))

(defn run []
  (judge real-generator-a real-generator-b 40000000))

(defn multiple-of? [n]
  (fn [i]
    (zero? (mod i n))))

(defn run-again []
  (let [generator-a #(filter (multiple-of? 4) (real-generator-a))
        generator-b #(filter (multiple-of? 8) (real-generator-b))]
    (judge generator-a generator-b 5000000)))
