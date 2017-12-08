(ns advent.six
  (:require [clojure.string :as str]))

(defn index-to-rebalance [banks]
  (let [most (apply max banks)]
    (ffirst (filter #(= most (last %))
                    (map-indexed vector banks)))))

(defn bank-index [banks index]
  (mod index (count banks)))

(defn rebalancing-state [banks]
  (let [max-index (index-to-rebalance banks)
        starting-index (bank-index banks (inc max-index))]
    {:index starting-index
     :banks (assoc banks max-index 0)
     :remaining (get banks max-index)}))

(defn next-rebalancing-state [{index :index
                               banks :banks
                               remaining :remaining}]
  (let [next-banks (update banks index inc)
        next-index (bank-index banks (inc index))
        remaining (dec remaining)]
    {:index next-index
     :banks next-banks
     :remaining remaining}))

(defn apply-n-times [n f]
  (apply comp (repeat n f)))

(defn rebalance-once [banks]
  (let [state (rebalancing-state banks)
        times (get state :remaining)
        rebalance-fn (apply-n-times times next-rebalancing-state)]
    (rebalance-fn state)))

(defn rebalance [banks]
  (loop [history #{}
         to-rebalance banks
         n 0]
    (if (contains? history to-rebalance)
      {:n n
       :banks to-rebalance}
      (recur (conj history to-rebalance)
             (get (rebalance-once to-rebalance) :banks)
             (inc n)))))

(defn read-input []
  (let [doc (slurp "resources/six.txt")
        line (first (str/split doc #"\n"))
        columns (str/split line #"\s+")]
    (mapv #(Integer/parseInt %) columns)))

(defn run []
  (get (rebalance (read-input))
       :n))
