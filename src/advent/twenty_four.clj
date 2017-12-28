(ns advent.twenty-four
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-line [line]
  (mapv #(Integer/parseInt %) (str/split line #"/")))

(defn read-input []
  (mapv read-line (str/split (slurp "resources/twenty_four.txt") #"\n")))

(defn build-components-catalog [components]
  (set components))

(def components-catalog (build-components-catalog (read-input)))

(defn initial-bridge []
  {:terminal 0
   :strength 0
   :length 0
   :finished? false
   :catalog components-catalog})

(defn has-strength? [strength [lhs rhs]]
  (or (= strength lhs)
      (= strength rhs)))

(defn components-with-strength [catalog strength]
  (set/select (partial has-strength? strength) catalog))

(defn component-options [{:keys [terminal catalog]}]
  (components-with-strength catalog terminal))

(defn use-connector [catalog [lhs rhs :as component]]
  (disj catalog component))

(defn augment-bridge [{:keys [strength length catalog terminal] :as bridge} component]
  (let [new-catalog (use-connector catalog component)
        [lhs rhs] component
        new-strength (apply + strength component)
        new-terminal (if (= lhs terminal) rhs lhs)
        new-length (inc length)]
    (assoc bridge
           :terminal new-terminal
           :last-used component
           :catalog new-catalog
           :length new-length
           :strength new-strength)))

(defn subsequent-bridges [bridges]
  (let [next-components (map component-options bridges)]
    (flatten (map (fn [bridge next-components]
                    (if (empty? next-components)
                      (assoc bridge :finished? true)
                      (map (partial augment-bridge bridge) next-components))) bridges next-components))))
(defn build-bridges []
  (loop [wip-bridges [(initial-bridge)]
         finished-bridges []]
    (if (empty? wip-bridges)
      finished-bridges
      (let [next-bridges (subsequent-bridges wip-bridges)
            still-wip (remove :finished? next-bridges)
            newly-finished (filter :finished? next-bridges)]
        (recur still-wip
               (into finished-bridges newly-finished))))))

(defn run []
  (:strength (last (sort-by :strength (build-bridges)))))

(defn run-again []
  (let [bridges (build-bridges)
        greatest-length (apply max (map :length bridges))
        longest-bridges (filter #(= greatest-length (:length %)) bridges)
        strongest-of-the-longest (last (sort-by :strength longest-bridges))]
    (:strength strongest-of-the-longest)))
