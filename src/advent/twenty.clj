(ns advent.twenty
  (:require [clojure.string :as str]))

(def axes [:x :y :z])

(defn extract-numbers [part]
  (mapv #(Integer/parseInt %) (re-seq #"[0-9\-]+" part)))

(defn particle-coordinates [particle]
  (mapv #(get-in particle [:position %]) axes))

(defn parse-line [index line]
  (let [[position velocity acceleration] (str/split line #", ")
        particle {:number index
                   :position (zipmap axes (extract-numbers position))
                   :velocity (zipmap axes (extract-numbers velocity))
                   :acceleration (zipmap axes (extract-numbers acceleration))}]
    (assoc particle :distances [(distance-from-origin particle)])))

(defn read-input []
  (map-indexed parse-line (str/split (slurp "resources/twenty.txt") #"\n")))

(defn direction-tick [particle direction]
  (let [position (get-in particle [:position direction])
        velocity (get-in particle [:velocity direction])
        acceleration (get-in particle [:acceleration direction])
        new-velocity (+ velocity acceleration)
        new-position (+ position new-velocity)]
    (-> particle
        (assoc-in [:position direction] new-position)
        (assoc-in [:velocity direction] new-velocity))))

(defn tick [particle]
  (let [moved (reduce direction-tick particle axes)
        distance (distance-from-origin moved)]
    (update moved :distances conj distance)))

(defn closest-to-origin [particles]
  (first (sort-by distance-from-origin particles)))

(defn run [needed-wins]
  (loop [particles (read-input)
         candidate (closest-to-origin particles)
         wins 0]
    (if (= wins needed-wins)
      (:number candidate)
      (let [new-particles (mapv tick particles)
            new-candidate (closest-to-origin new-particles)]
        (if (= (:number new-candidate) (:number candidate))
          (recur new-particles
                 new-candidate
                 (inc wins))
          (recur new-particles
                 new-candidate
                 0))))))

;; find the particle which is closest 250 ticks in a row, which seems
;; like it ought to be enough???
(defn which-stays-closest? []
  (run 250))
