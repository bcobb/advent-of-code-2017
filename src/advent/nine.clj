(ns advent.nine
  (:require [clojure.string :as str]))

(def garbage-ignore-pattern #"!.")
(def bracket-pattern #"([<>\{\}])")

(def space-surrounding " $1 ")
(def nothing "")

(defn tokenize [string]
  (let [tokens (-> string
                   (str/replace garbage-ignore-pattern nothing)
                   (str/replace bracket-pattern space-surrounding)
                   (str/split #" "))]
    (remove empty? tokens)))

(defmulti advance (fn [state token] [(:mode state) token]))

(defmethod advance [:group "{"] [state _]
  (update state :stack conj "{"))

(defmethod advance [:group "}"] [{stack :stack :as state} _]
  (-> state
      (update :score + (count stack))
      (update :stack pop)))

(defmethod advance [:group "<"] [state _]
  (assoc state :mode :garbage))

(defmethod advance [:garbage ">"] [state _]
  (assoc state :mode :group))

(defmethod advance :default [state _]
  state)

(defn initial-state []
  {:score 0
   :mode :group
   :stack []})

(defn interpret [string]
  (let [tokens (tokenize string)]
    (reduce advance (initial-state) tokens)))

(defn run []
  (interpret (slurp "resources/nine.txt")))
