(ns advent.twelve
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[program connection-string] (str/split line #" <-> ")
        connections (conj (str/split connection-string #", ")
                          program)]
    (map vector (repeat program) connections)))

(def conj* (fnil conj #{}))

(defn add-connection [state [from to]]
  (-> state
      (update from conj* to)
      (update to conj* from)))

(defn update-state [state connections]
  (reduce add-connection state connections))

(defn connections-to [program state]
  (let [immediate-connections (get state program)]
    (loop [connections immediate-connections
           programs immediate-connections]
      (if (empty? programs)
        connections
        (let [secondary-connections (reduce set/union (map (partial get state) programs))
              unknown-connections (set/difference secondary-connections connections)]
          (recur (set/union connections secondary-connections)
                 unknown-connections))))))

(defn read-input []
  (map parse-line (str/split (slurp "resources/twelve.txt") #"\n")))

(defn run []
  (let [state (reduce update-state {} (read-input))]
    (count (connections-to "0" state))))
