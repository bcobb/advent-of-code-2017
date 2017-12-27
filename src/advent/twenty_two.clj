(ns advent.twenty-two
  (:require [clojure.string :as str]))

(def infected-moves {:north :east
                     :east :south
                     :south :west
                     :west :north})

(def clean-moves {:north :west
                  :west :south
                  :south :east
                  :east :north})

(def direction->coordinate-change {:north [-1 0]
                                   :east [0 1]
                                   :south [1 0]
                                   :west [0 -1]})

(defn infected-symbol? [c]
  (= \# c))

(defn detect-infected [grid]
  (reduce into (map-indexed
                (fn [x row]
                  (vec (remove nil? (map-indexed
                                     (fn [y node]
                                       (when (infected-symbol? node)
                                         [x y]))
                                     row))))
                grid)))

(defn read-input []
  (let [grid (mapv vec (str/split (slurp "resources/twenty_two.txt") #"\n"))]
    {:initial-size (count grid)
     :infections 0
     :infected (set (detect-infected grid))}))

(defn starting-point [{side-length :initial-size}]
  (let [midpoint (int (/ side-length 2))]
    [midpoint midpoint]))

(defn initial-state []
  (let [grid (read-input)]
    (assoc grid
           :direction :north
           :position (starting-point grid))))

(defn on-infected-node? [{:keys [position infected]}]
  (contains? infected position))

(defn direction-map [state]
  (if (on-infected-node? state)
    infected-moves
    clean-moves))

(defn update-direction [{:keys [direction] :as state}]
  (let [mapping (direction-map state)]
    (update state :direction (partial get mapping))))

(defn update-position [{:keys [direction position] :as state}]
  (let [[dx dy] (get direction->coordinate-change direction)]
    (-> state
        (update-in [:position 0] + dx)
        (update-in [:position 1] + dy))))

(defn update-infected [{:keys [infected position] :as state}]
  (if (on-infected-node? state)
    (update state :infected disj position)
    (-> state
        (update :infected conj position)
        (update :infections inc))))

(defn burst [state]
  (-> state
      update-direction
      update-infected
      update-position))

(defn simulate [n]
  (last (take (inc n) (iterate burst (initial-state)))))
