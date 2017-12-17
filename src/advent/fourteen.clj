(ns advent.fourteen
  (:require [clojure.string :as str]
            [advent.ten :refer [knot-hash]]))

(def char->binary {\0 "0000"
                   \1 "0001"
                   \2 "0010"
                   \3 "0011"
                   \4 "0100"
                   \5 "0101"
                   \6 "0110"
                   \7 "0111"
                   \8 "1000"
                   \9 "1001"
                   \a "1010"
                   \b "1011"
                   \c "1100"
                   \d "1101"
                   \e "1110"
                   \f "1111"})

(defn string->binary [string]
  (str/join (map char->binary string)))

(defn subkeys-for-key [keystring]
  (map (partial str keystring "-")
       (range 128)))

(defn grid [keystring]
  (map (comp string->binary knot-hash)
       (subkeys-for-key keystring)))

(defn used [row]
  (count (filter (partial = \1)
                 row)))

(defn run []
  (reduce + (map used (grid "ffayrhll"))))
