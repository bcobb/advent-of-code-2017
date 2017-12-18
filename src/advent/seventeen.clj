(ns advent.seventeen
  (:require [clojure.string :as str]))

(defn make-state [steps]
  {:buffer [0]
   :position 0
   :next 1
   :steps steps})

(defn step [{buffer :buffer
             steps :steps :as state}]
  (let [length (count buffer)]
    (update state :position #(mod (+ steps %) length))))


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
