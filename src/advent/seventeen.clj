(ns advent.seventeen
  (:require [clojure.string :as str]))

(defn make-state [steps]
  {:buffer [0]
   :position 0
   :next 1
   :steps steps})

(defn make-mini-state []
  {:length 1
   :inserted 0
   :position 0})

(defn step [{buffer :buffer
             steps :steps :as state}]
  (let [length (count buffer)]
    (update state :position #(mod (+ steps %) length))))

(defn mini-stepper [steps]
  (fn [{length :length :as state}]
    (-> state
        (update :position #(mod (+ steps %) length))
        (update :position inc)
        (update :inserted inc)
        (update :length inc))))

(defn at-position? [n]
  (fn [{position :position}]
    (= position n)))

(defn not-inserted? [n]
  (fn [{inserted :inserted}]
    (<= inserted n)))

(defn insert [v index value]
  (let [before (subvec v 0 (inc index))
        after (subvec v (inc index))]
    (-> before
        (into [value])
        (into after))))

(defn evolve [state]
  (let [{position :position
         next :next :as moved-state} (step state)]
    (-> moved-state
        (update :buffer #(insert % position next))
        (update :position inc)
        (update :next inc))))

(defn display [{position :position
                buffer :buffer}]
  (str/join " "
            (map-indexed (fn [i v]
                           (if (= i position)
                             (str "(" v ")")
                             (str v)))
                         buffer)))

(defn next-value [{position :position
                   buffer :buffer}]
  (nth buffer (inc position)))

(defn run []
  (next-value (last (take 2018 (iterate evolve (make-state 356))))))

(defn run-again []
  (let [steps (iterate (mini-stepper 356) (make-mini-state))
        relevant-steps (take-while (not-inserted? 50000000) steps)
        insertions-after-zero (filter (at-position? 1) relevant-steps)]
    (:inserted (last insertions-after-zero))))
