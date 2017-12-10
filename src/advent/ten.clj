(ns advent.ten
  (:require [clojure.string :as str]))

(defn wrap+ [max]
  (fn [& summands]
    (mod (apply + summands)
         max)))

(defn vector-slice [v start length]
  (let [end (+ start length)]
    (if (> end (count v))
      (let [remainder (- end (count v))
            indices (concat (range start (count v))
                            (range remainder))]
        {:list v
         :indices indices})
      (let [indices (range start end)]
        {:list v
         :indices indices}))))

(defn reverse-slice [{list :list
                      indices :indices}]
  (let [transformations (zipmap indices (reverse indices))]
    (reduce (fn [l me]
              (let [from (key me)
                    to (val me)
                    val (nth list from)]
                (assoc l to val)))
            list
            transformations)))

(defn take-slice [n {list :list}]
  (take n list))

(defn initial-state [list]
  {:list list
   :position 0
   :skip 0})

(defn process-length [{position :position
                       list :list
                       skip :skip :as state} length]
  (let [new-list (reverse-slice (vector-slice list position length))]
    (-> state
        (update :position (wrap+ (count list)) length skip)
        (update :skip inc)
        (assoc :list new-list))))

(defn process-lengths [state lengths]
  (reduce process-length state lengths))

(defn dense-hash [{list :list}]
  (map (partial apply bit-xor) (partition 16 list)))

(defn to-hex [i]
  (let [value (Integer/toHexString i)]
    (if (= 1 (count value))
      (str "0" value)
      value)))

(defn knot-hash [state]
  (apply str (map to-hex (dense-hash state))))

(defn run []
  (let [list (vec (range 256))
        lengths (mapv #(Integer/parseInt %)
                      (str/split (slurp "resources/ten.txt") #","))
        result (process-lengths (initial-state list) lengths)]
    (apply * (take-slice 2 result))))

(defn run-again []
  (let [list (vec (range 256))
        addendum [17 31 73 47 23]
        input-lengths (mapv int (slurp "resources/ten.txt"))
        combined-lengths (into input-lengths addendum)
        lengths (vec (apply concat (repeat 64 combined-lengths)))
        result (process-lengths (initial-state list) lengths)]
    (knot-hash result)))
