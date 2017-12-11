(ns advent.eleven
  (:require [clojure.string :as str]))

(defn initial-grid []
  {:tiles []})

(defn initial-state []
  {:x 0
   :y 0
   :z 0
   :max-away 0})

(def origin (initial-state))

(def move->position-change {"n" [0 1 -1]
                            "ne" [1 0 -1]
                            "se" [1 -1 0]
                            "s" [0 -1 1]
                            "sw" [-1 0 1]
                            "nw" [-1 1 0]})

(defn apply-move [state move]
  (let [[dx dy dz] (get move->position-change move ::unknown)]
    (-> state
        (update :x + dx)
        (update :y + dy)
        (update :z + dz))))

(defn distance-from-origin [point]
  (apply max (map #(Math/abs %) (vals point))))

(defn apply-move-with-memory [state move]
  (let [after-move (apply-move state move)
        distance-away (distance-from-origin after-move)]
    (update after-move :max-away max distance-away)))

(defn input->moves [input]
  (str/split input #","))

(defn run []
  (let [moves (input->moves (slurp "resources/eleven.txt"))]
    (distance-from-origin (reduce apply-move origin moves))))

(defn run-again []
  (let [moves (input->moves (slurp "resources/eleven.txt"))]
    (:max-away (reduce apply-move-with-memory origin moves))))
