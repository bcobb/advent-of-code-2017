(ns advent.one)

(def input (atom nil))

(defn read-input []
  (or @input
      (reset! input
              (map #(Integer/parseInt (str %)) (slurp "resources/one.txt")))))

(defn nth' [coll index]
  (nth coll (mod index (count coll))))

(defn pairs
  ([coll]
   (pairs coll dec))
  ([coll jump-fn]
   (map #(vector (nth' coll (jump-fn %))
                 (nth' coll %))
        (range (count coll)))))

(defn pairwise-sum [coll]
  (->> coll
       pairs
       (filter #(apply = %))
       (map first)
       (reduce +)))

(defn part-two-sum [coll]
  (let [halfway (/ (count coll) 2)]
    (->> (pairs coll #(+ % halfway))
         (filter #(apply = %))
         (map first)
         (reduce +))))

(defn run []
  (pairwise-sum (read-input)))

(defn run-again []
  (part-two-sum (read-input)))
