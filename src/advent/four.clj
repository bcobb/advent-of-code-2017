(ns advent.four
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn lines [doc]
  (str/split doc #"\n"))

(defn passphrase [line]
  (str/split line #"\s+"))

(defn no-repeats? [passphrase]
  (= (count passphrase)
     (count (set passphrase))))

(defn letter-histogram [word]
  (reduce
   (fn [a c] (update a c (fnil inc 0)))
   {}
   word))

(defn anagram? [a b]
  (= (letter-histogram a)
     (letter-histogram b)))

(defn passphrase-has-no-anagrams? [passphrase]
  (empty? (filter (partial apply anagram?)
                  (combo/combinations passphrase 2))))

(defn run []
  (let [input (slurp "resources/four.txt")
        passphrases (map passphrase (lines input))]
    (count (filter no-repeats? passphrases))))

(defn run-again []
  (let [input (slurp "resources/four.txt")
        passphrases (map passphrase (lines input))]
    (count (filter passphrase-has-no-anagrams? passphrases))))
