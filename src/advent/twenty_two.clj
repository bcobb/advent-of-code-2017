(ns advent.twenty-two
  (:require [clojure.string :as str]))

(def direction->coordinate-change {:north [-1 0]
                                   :east [0 1]
                                   :south [1 0]
                                   :west [0 -1]})

(def node-state-transitions {:clean :weakened
                             :weakened :infected
                             :infected :flagged
                             :flagged :clean})

(def node-state->direction {:clean :left
                            :weakened :same
                            :infected :right
                            :flagged :reverse})

(def cardinal->relative->cardinal {:north {:left :west
                                           :same :north
                                           :right :east
                                           :reverse :south}
                                   :east {:left :north
                                          :same :east
                                          :right :south
                                          :reverse :west}
                                   :south {:left :east
                                           :same :south
                                           :right :west
                                           :reverse :north}
                                   :west {:left :south
                                          :same :west
                                          :right :north
                                          :reverse :east}})

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
     :weakened #{}
     :flagged #{}
     :infected (set (detect-infected grid))}))

(defn starting-point [{side-length :initial-size}]
  (let [midpoint (int (/ side-length 2))]
    [midpoint midpoint]))

(defn initial-state []
  (let [grid (read-input)]
    (assoc grid
           :direction :north
           :position (starting-point grid))))

(defn node-state [{:keys [position infected weakened flagged]}]
  (condp contains? position
    infected :infected
    weakened :weakened
    flagged :flagged
    :clean))

(defn update-direction [{:keys [direction] :as state}]
  (let [node-state (node-state state)
        relative-direction (get node-state->direction node-state)
        new-direction (get-in cardinal->relative->cardinal [direction relative-direction])]
    (assoc state :direction new-direction)))

(defn update-position [{:keys [direction position] :as state}]
  (let [[dx dy] (get direction->coordinate-change direction)]
    (-> state
        (update-in [:position 0] + dx)
        (update-in [:position 1] + dy))))

(defn count-infections [state new-node-state]
  (if (= :infected new-node-state)
    (update state :infections inc)
    state))

(defn update-node-state [{:keys [position] :as state}]
  (let [current-node-state (node-state state)
        new-node-state (get node-state-transitions current-node-state)]
    (-> state
        (update current-node-state disj position)
        (update new-node-state conj position)
        (count-infections new-node-state)
        (dissoc :clean))))

(defn burst [state]
  (-> state
      update-direction
      update-node-state
      update-position))

(defn simulate [n]
  (last (take (inc n) (iterate burst (initial-state)))))
