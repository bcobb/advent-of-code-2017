(ns advent.five
  (:require [clojure.string :as str]))

(defn initial-state [offsets]
  {:position 0
   :offsets offsets})

(defn escaped? [{position :position
                 offsets :offsets}]
  (or (neg? position)
      (<= (count offsets) position)))

(defn new-offsets [offsets last-position]
  (update offsets last-position inc))

(defn move-once [{position :position
                  offsets :offsets}]
  (let [jump (nth offsets position)
        new-position (+ position jump)]
    {:offsets (new-offsets offsets position)
     :position new-position}))

(defn steps-to-exit [offsets]
  (loop [n 0
         state (initial-state offsets)]
    (if (escaped? state)
      n
      (recur (inc n)
             (move-once state)))))

(defn read-input []
  (let [lines (str/split (slurp "resources/five.txt") #"\n")]
    (mapv #(Integer/parseInt %) lines)))

(defn run []
  (steps-to-exit (read-input)))
