(ns advent.seven
  (:require [clojure.string :as str]))

(def parent-pattern #"([^ ]+) \((\d+)\)")

(defn parse-line [line]
  (let [[parent-data children-data] (str/split line #" -> ")
        children (str/split (str children-data) #", ")
        [parent weight] (rest (re-matches parent-pattern parent-data))]
    {:parent parent
     :weight (Integer/parseInt weight)
     :children (vec (remove empty? children))}))

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
  (filter #(none-in? (val %)) (build-metadata lines)))

(defn read-input []
  (str/split (slurp "resources/seven.txt") #"\n"))

(defn run []
  (bottom-program (read-input)))
