(ns advent.eight
  (:require [clojure.string :as str]))

(def instruction-pattern #"([^ ]+) (inc|dec) ([^ ]+) if ([^ ]+) ([^ ]+) ([^ ]+)")

(def predicate->fn {">"  (fnil > 0)
                    ">=" (fnil >= 0)
                    "<"  (fnil < 0)
                    "<=" (fnil <= 0)
                    "==" (fnil = 0)
                    "!=" (fnil not= 0)})

(def op->fn {"inc" (fnil + 0)
             "dec" (fnil - 0)})

(def zero-or-identity (fnil identity 0))

(defn parse-instruction [instruction]
  (let [parts (rest (re-matches instruction-pattern instruction))
        [var op amount conditional-var predicate conditional-val] parts]
    {:var var
     :op op
     :amount (Integer/parseInt amount)
     :conditional-var conditional-var
     :predicate predicate
     :conditional-val (Integer/parseInt conditional-val)}))

(defn instruction->conditional [{var :conditional-var
                                 predicate :predicate
                                 conditional-val :conditional-val}]
  (fn [register]
    (let [actual-val (get register var)
          predicate-fn (get predicate->fn predicate)]
      (predicate-fn actual-val conditional-val))))

(defn instruction->operation [{var :var
                               op :op
                               amount :amount
                               :as instruction}]
  (fn [register]
    (let [operation (get op->fn op)
          conditional (instruction->conditional instruction)]
      (if (conditional register)
        (update register var operation amount)
        (update register var zero-or-identity)))))

(defn apply-instruction [register instruction]
  (let [operation (instruction->operation instruction)]
    (operation register)))

(defn apply-instruction-with-memory-of-maxima [state instruction]
  (let [register (apply-instruction (get state :register) instruction)
        current-max (apply max (vals register))]
    (-> state
        (assoc :register register)
        (update :maxima conj current-max))))

(defn read-input []
  (str/split (slurp "resources/eight.txt") #"\n"))

(defn run []
  (let [register (reduce apply-instruction {} (map parse-instruction (read-input)))]
    (apply max (vals register))))

(defn run-again []
  (let [state (reduce apply-instruction-with-memory-of-maxima {} (map parse-instruction (read-input)))]
    (apply max (get state :maxima))))
