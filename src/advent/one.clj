(ns advent.one)

(def input (atom nil))

(defn read-input []
  (or @input
      (reset! input
              (map #(Integer/parseInt (str %)) (slurp "resources/one.txt")))))

(defn nth' [coll index]
  (nth coll (mod index (count coll))))

(defn pairs [coll]
  (map #(vector (nth' coll (dec %))
                (nth' coll %))
       (range (count coll))))

(defn pairwise-sum [coll]
  (->> coll
       pairs
       (filter #(apply = %))
       (map first)
       (reduce +)))

(defn run []
  (pairwise-sum (read-input)))
