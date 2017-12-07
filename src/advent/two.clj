(ns advent.two
  (:require [clojure.string :as str]))

(defn line-to-numbers [line]
  (map (fn [char] (Integer/parseInt (str char)))
       (str/split line #"\s+")))

(defn read-document [doc]
  (map line-to-numbers (str/split doc #"\n")))

(defn read-input []
  (read-document (slurp "resources/two.txt")))

(defn checksum-part [row]
  (let [upper (apply max row)
        lower (apply min row)]
    (- upper lower)))

(defn checksum [rows]
  (reduce + (map checksum-part rows)))

(defn run []
  (checksum (read-input)))
