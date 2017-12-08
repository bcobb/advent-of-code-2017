(ns advent.five
  (:require [clojure.string :as str]))

(defn initial-state [offsets]
  {:position 0
   :offsets offsets})

(defn escaped? [{position :position
                 offsets :offsets}]
  (or (neg? position)
      (<= (count offsets) position)))

(defn simple-offset [offsets last-position]
  (update offsets last-position inc))

(defn complex-adjustment [offset]
  (if (> offset 2)
    (dec offset)
    (inc offset)))

(defn complex-offset [offsets last-position]
  (update offsets last-position complex-adjustment))

(defn move-once
  ([state]
   (move-once state simple-offset))
  ([{position :position
     offsets :offsets}
    new-offsets-fn]
   (let [jump (nth offsets position)
         new-position (+ position jump)]
     {:offsets (new-offsets-fn offsets position)
      :position new-position})))

(defn steps-to-exit [offsets new-offsets-fn]
  (loop [n 0
         state (initial-state offsets)]
    (if (escaped? state)
      n
      (recur (inc n)
             (move-once state new-offsets-fn)))))

(defn read-input []
  (let [lines (str/split (slurp "resources/five.txt") #"\n")]
    (mapv #(Integer/parseInt %) lines)))

(defn run []
  (steps-to-exit (read-input) simple-offset))

(defn run-again []
  (steps-to-exit (read-input) complex-offset))
