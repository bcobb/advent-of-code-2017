(ns advent.fourteen
  (:require [clojure.string :as str]
            [advent.ten :refer [knot-hash]]
            [clojure.set :as set]))

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
  (mapv (partial str keystring "-")
        (range 128)))

(defn grid [keystring]
  (mapv (comp string->binary knot-hash)
        (subkeys-for-key keystring)))

(defn used [row]
  (count (filter (partial = \1)
                 row)))

(defn run []
  (reduce + (map used (grid "ffayrhll"))))

(defn all-coords [grid]
  (let [max-x (count grid)
        max-y (count (first grid))]
    (reduce concat (mapv #(mapv (partial vector %) (range max-y))
                         (range max-x)))))

(defn on? [grid coords]
  (= \1 (get-in grid coords)))

(defn off? [grid coords]
  (= \0 (get-in grid coords)))

(defn apply-move [[x y] [dx dy]]
  [(+ x dx)
   (+ y dy)])

(defn search-coords [grid coords]
  (let [relative-moves [[1 0]
                        [-1 0]
                        [0 1]
                        [0 -1]
                        [0 0]]
        moves (map (partial apply-move coords)
                   relative-moves)
        max-x (count grid)
        max-y (count (first grid))]
    (filter (fn [[x y]]
              (and (< -1 x max-x)
                   (< -1 y max-y))) moves)))

(defn neighbors [grid coords]
  (let [neighbor-coords (search-coords grid coords)]
    (set (filter (partial on? grid)
                 neighbor-coords))))

(defn subregion [grid coords]
  (let [immediate-neighbors (disj (neighbors grid coords) coords)]
    (loop [result #{coords}
           frontier (first immediate-neighbors)
           remaining (disj immediate-neighbors frontier)]
      (if (nil? frontier)
        result
        (let [new-neighbors (neighbors grid frontier)
              new-result (conj result frontier)
              initial-remaining (set/union new-neighbors remaining)
              new-remaining (set/difference initial-remaining result)
              new-frontier (first new-remaining)]
          (recur new-result new-frontier new-remaining))))))

(defn all-regions [grid]
  (let [frontiers (filter (partial on? grid) (all-coords grid))]
    (loop [frontier (first frontiers)
           remaining (disj (set frontiers) frontier)
           regions []]
      (if (nil? frontier)
        regions
        (let [region (subregion grid frontier)
              initial-remaining (set/difference remaining region)
              new-frontier (first initial-remaining)
              new-remaining (disj initial-remaining new-frontier)]
          (recur new-frontier new-remaining (conj regions region)))))))

(defn paintable [grid]
  (mapv vec grid))

(defn paint [grid colors]
  (let [grid' (paintable grid)
        whole (all-coords grid)
        canvas (reduce #(assoc-in %1 %2 ".") grid' whole)
        painting (reduce #(assoc-in %1 %2 "#") canvas colors)]
    (str/join "\n" (map (partial apply str) painting))))

(defn run-again [keystring]
  (count (all-regions (grid keystring))))
