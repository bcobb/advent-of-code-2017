(ns advent.twenty-one
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn pattern-to-parts [pattern]
  (str/split pattern #" => "))

(defn part-to-lines [part]
  (str/split part #"\/"))

(defn lines-to-grids [lines]
  (mapv vec lines))

(defn read-input []
  (map (comp
        (partial mapv lines-to-grids)
        (partial mapv part-to-lines)
        pattern-to-parts) (str/split (slurp "resources/twenty_one.txt") #"\n")))

(def matrix-3->plane {0 -1
                      1 0
                      2 1})

(def matrix-2->plane {0 -1
                      1 1})

(def plane->matrix-3 (set/map-invert matrix-3->plane))

(def plane->matrix-2 (set/map-invert matrix-2->plane))

(defn project-3 [coords]
  (mapv matrix-3->plane coords))

(defn project-2 [coords]
  (mapv matrix-2->plane coords))

(defn unproject-3 [coords]
  (mapv plane->matrix-3 coords))

(defn unproject-2 [coords]
  (mapv plane->matrix-2 coords))

(defn rotate-projected [[x y]]
  [y (* -1 x)])

(defn flip-horizontal-projected-3 [[x y]]
  [x (* -1 y)])

(defn flip-vertical-projected-3 [[x y]]
  [(* -1 x) y])

(defn all-coordinates [matrix]
  (let [max-coordinate (count matrix)]
    (combo/cartesian-product (range max-coordinate) (range max-coordinate))))

(defn matrix-transformer [xform]
  (fn [matrix]
    (let [coordinates (all-coordinates matrix)
          pixels (map (partial get-in matrix) coordinates)
          transform (comp unproject-3 xform project-3)
          new-coordinates (map transform coordinates)
          transforms (map vector new-coordinates pixels)]
      (reduce (partial apply assoc-in) matrix transforms))))

(def rotate-3 (matrix-transformer rotate-projected-3))
(def flip-horizontal-3 (matrix-transformer flip-horizontal-projected-3))
(def flip-vertical-3 (matrix-transformer flip-vertical-projected-3))

(defn translate-vertical [n [x y]]
  [(+ x n) y])

(defn translate-horizontal [n [x y]]
  [x (+ y n)])

(defn inspect-matrix [matrix]
  (str/join "\n" (map #(str/join " " %) matrix)))

(defn matrix-permutations [matrix]
  (let [rotations (take 4 (iterate rotate-3 matrix))
        horizontal-flips (map flip-horizontal-3 rotations)
        vertical-flips (map flip-vertical-3 rotations)]
    (set (concat rotations horizontal-flips vertical-flips))))

(def initial-matrix (lines-to-grids (part-to-lines ".#./..#/###")))

(defn read-pattern-library []
  (let [basis (read-input)
        canonical-patterns (map first basis)
        enhancements (map last basis)]
    (zipmap canonical-patterns enhancements)))

(defn enhancement [pattern-library matrix]
  (let [options (matrix-permutations matrix)
        canonical-pattern (first (filter (partial get pattern-library) options))]
    (get pattern-library canonical-pattern)))
