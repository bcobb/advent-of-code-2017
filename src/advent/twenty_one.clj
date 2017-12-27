(ns advent.twenty-one
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.zip :as zip]))

(defn flat-map [f coll]
  (reduce (fn [coll' el] (apply conj coll' (f el)))
          []
          coll))

(defn partitionv [n coll]
  (mapv vec (partition n coll)))

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

(defmulti project (fn [matrix coords] (count matrix)))

(defmethod project 2 [_ coords]
  (mapv matrix-2->plane coords))

(defmethod project 3 [_ coords]
  (mapv matrix-3->plane coords))

(defmulti unproject (fn [matrix coords] (count matrix)))

(defmethod unproject 2 [_ coords]
  (mapv plane->matrix-2 coords))

(defmethod unproject 3 [_ coords]
  (mapv plane->matrix-3 coords))

(defn rotate-projected [[x y]]
  [y (* -1 x)])

(defn flip-horizontal-projected [[x y]]
  [x (* -1 y)])

(defn flip-vertical-projected [[x y]]
  [(* -1 x) y])

(defn all-coordinates [matrix]
  (let [max-coordinate (count matrix)]
    (combo/cartesian-product (range max-coordinate) (range max-coordinate))))

(defn matrix-transformer [xform]
  (fn [matrix]
    (let [project* (partial project matrix)
          unproject* (partial unproject matrix)
          coordinates (all-coordinates matrix)
          pixels (map (partial get-in matrix) coordinates)
          transform (comp unproject* xform project*)
          new-coordinates (map transform coordinates)
          transforms (map vector new-coordinates pixels)]
      (reduce (partial apply assoc-in) matrix transforms))))

(def rotate (matrix-transformer rotate-projected))
(def flip-horizontal (matrix-transformer flip-horizontal-projected))
(def flip-vertical (matrix-transformer flip-vertical-projected))

(defn inspect-matrix [matrix]
  (str/join "\n" (map #(str/join " " %) matrix)))

(defn matrix-permutations [matrix]
  (let [rotations (take 4 (iterate rotate matrix))
        horizontal-flips (map flip-horizontal rotations)
        vertical-flips (map flip-vertical rotations)]
    (set (concat rotations horizontal-flips vertical-flips))))

(def initial-matrix (lines-to-grids (part-to-lines ".#./..#/###")))

(defn read-pattern-library []
  (let [basis (read-input)
        canonical-patterns (map first basis)
        enhancements (map last basis)]
    (zipmap canonical-patterns enhancements)))

(defn canonical-pattern [pattern-library matrix]
  (let [options (matrix-permutations matrix)]
    (first (filter (partial get pattern-library) options))))

(def canonical (partial canonical-pattern (read-pattern-library)))

(defn enhancement [pattern-library matrix]
  (get pattern-library (canonical-pattern pattern-library matrix)))

(def enhance-submatrix (partial enhancement (read-pattern-library)))

;; takes a matrix, splits each row into vectors of size n
(defn split-rows-to-size [n matrix]
  (mapv #(partitionv n %) matrix))

;; takes a matrix, returns a matrix with an additional layer of nesting around each n row vectors
;; matrix* implies that this generally takes a matrix that's had its row structure modified
(defn group-rows-to-size [n matrix*]
  (partitionv n matrix*))

;; takes a matrix which has had its rows split to size and its row vectors
;; grouped so that the first elements of each group together form a submatrix,
;; as do the second, third, etc elements
(defn collect-submatrices-of-size [n matrix*]
  (mapv #(partitionv n (apply interleave %)) matrix*))

(defn detect-submatrices-of-size [submatrix-size matrix]
  (->> matrix
       (split-rows-to-size submatrix-size)
       (group-rows-to-size submatrix-size)
       (collect-submatrices-of-size submatrix-size)
       (flat-map identity)))

;; order is important - only find submatrices of size 3 if it would be impossible to find submatrices of size 2
(defmulti submatrices (fn [matrix] (if (zero? (mod (count matrix) 2))
                                     2
                                     3)))

(defmethod submatrices 3 [matrix]
  (detect-submatrices-of-size 3 matrix))

(defmethod submatrices 2 [matrix]
  (detect-submatrices-of-size 2 matrix))

;; enhances each submatrix of the given matrix, and returns the whole
;; enhanced matrix by some dark, dark magic
(defn enhance [matrix]
  (let [submatrices (submatrices matrix)
        enhanced-submatrices (mapv enhance-submatrix submatrices)
        side-length (int (Math/sqrt (count submatrices)))]
    (->> enhanced-submatrices
         (partitionv side-length)
         (mapv (partial apply interleave))
         (flat-map (partial partitionv side-length))
         (mapv (comp vec flatten)))))

(defn iterate-to [n]
  (last (take (inc n) (iterate enhance initial-matrix))))

(defn on? [e]
  (= \# e))

(defn on-after [n]
  (count (filter on? (flatten (iterate-to n)))))
