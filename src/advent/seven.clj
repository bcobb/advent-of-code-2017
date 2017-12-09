(ns advent.seven
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.set :as set]))

(def parent-pattern #"([^ ]+) \((\d+)\)")

(defn parse-line [line]
  (let [[parent-data children-data] (str/split line #" -> ")
        children (str/split (str children-data) #", ")
        [parent weight] (rest (re-matches parent-pattern parent-data))]
    {:parent parent
     :weight (Integer/parseInt weight)
     :children (set (remove empty? children))}))

(def set-or-increment (fnil inc 0))
(def zero-or-identity (fnil identity 0))

(defn update-metadata-with-relationship [metadata parent child]
  (-> metadata
      (update-in [parent :in] zero-or-identity)
      (update-in [parent :out] set-or-increment)
      (update-in [child :in] set-or-increment)
      (update-in [child :out] zero-or-identity)))

(defn update-metadata-with-parent [metadata parent]
  (-> metadata
      (update-in [parent :in] zero-or-identity)
      (update-in [parent :out] zero-or-identity)))

(defn update-degree-metadata [metadata {parent :parent
                                        children :children}]
  (let [metadata-with-parent (update-metadata-with-parent metadata parent)]
    (reduce #(update-metadata-with-relationship %1 parent %2)
            metadata-with-parent
            children)))

(defn none-in? [{in :in}]
  (zero? in))

(defn build-metadata [lines]
  (reduce update-degree-metadata {} (map parse-line lines)))

(defn bottom-program [lines]
  (ffirst (filter #(none-in? (val %)) (build-metadata lines))))

(defn read-input []
  (str/split (slurp "resources/seven.txt") #"\n"))

(defn run []
  (bottom-program (read-input)))

;; Part 2

(defn update-weight [weights {parent :parent
                              weight :weight}]
  (assoc weights parent weight))

(defn lines-to-weights [lines]
  (reduce update-weight {} (map parse-line lines)))

(defn lines-to-map [lines]
  (reduce #(assoc %1 (get %2 :parent) (get %2 :children))
          {}
          (map parse-line lines)))

(defn weight [program->weight parent->children program]
  (loop [total (get program->weight program)
         children (get parent->children program)]
    (if (empty? children)
      total
      (let [child (first children)]
        (recur (+ total (get program->weight child))
               (concat (get parent->children child) (rest children)))))))

(defn balanced? [program->weight parent->children start]
  (let [children (get parent->children start)]
    (or (empty? children)
        (apply = (map (partial weight program->weight parent->children) (get parent->children start))))))

(defn siblings [parent->children program]
  (let [siblings-plus-me (last (last (filter #(contains? (set (val %)) program) parent->children)))]
    (disj siblings-plus-me program)))

(defn parent-of? [parent->children a b]
  (loop [possible-children (get parent->children a)]
    (if (empty? possible-children)
      false
      (if (contains? possible-children b)
        true
        (recur (reduce set/union (map #(get parent->children %) possible-children)))))))

(defn find-unbalanced-programs []
  (let [parent->children (lines-to-map (read-input))
        program->weight (lines-to-weights (read-input))
        programs (keys parent->children)
        unbalanced-programs (set (remove (partial balanced? program->weight parent->children) programs))
        bottom (bottom-program (read-input))]
    (disj unbalanced-programs bottom)))

(defn debug-unbalanced [program]
  (let [parent->children (lines-to-map (read-input))
        program->weight (lines-to-weights (read-input))]
    {:program program
     :weight (get program->weight program)
     :total-weight (weight program->weight parent->children program)
     :children (mapv lel (get parent->children program))}))

;; find-unbalanced-programs returns two
;; ignore the parent program
;; debug the other one, and find the answer by inspection
;; demerit to me for not doing it algorithmically!
