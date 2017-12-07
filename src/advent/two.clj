(ns advent.two
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as c]))

(defn line-to-numbers [line]
  (map (fn [char] (Integer/parseInt (str char)))
       (str/split line #"\s+")))

(defn read-document [doc]
  (map line-to-numbers (str/split doc #"\n")))

(defn read-input []
  (read-document (slurp "resources/two.txt")))

(defn min-plus-max [row]
  (let [upper (apply max row)
        lower (apply min row)]
    (- upper lower)))

(defn divisible? [a b]
  (or
   (zero? (mod a b))
   (zero? (mod b a))))

(defn quotient [a b]
  (let [numerator (max a b)
        denominator (min a b)]
    (/ numerator denominator)))

(defn quotient-of-divisible-items [row]
  (let [combos (c/combinations row 2)
        candidates (filter (partial apply divisible?) combos)]
    (assert (= 1 (count candidates)))
    (apply quotient (first candidates))))

(defn checksum
  [rows row-checksum-fn]
  (reduce + (map row-checksum-fn rows)))

(defn run []
  (checksum (read-input) min-plus-max))

(defn run-again []
  (checksum (read-input) quotient-of-divisible-items))
