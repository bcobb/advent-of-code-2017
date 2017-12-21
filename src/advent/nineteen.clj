(ns advent.nineteen
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input []
  (mapv vec (str/split (slurp "resources/nineteen.txt") #"\n")))

(defn vertical? [char]
  (= \| char))

(defn corner? [char]
  (= \+ char))

(defn horizontal? [char]
  (= \- char))

(defn letter? [char]
  (re-find #"[A-Za-z]" (str char)))

(defn pathway? [char]
  (or
   (vertical? char)
   (corner? char)
   (horizontal? char)
   (letter? char)))

(defn entry-point [matrix]
  (let [top-row (first matrix)
        column (ffirst (filter (comp vertical? last) (map-indexed vector top-row)))]
    [0 column]))

(defn on-matrix? [matrix [x y]]
  (let [max-x (dec (count matrix))
        max-y (dec (count (first matrix)))]
    (and (<= 0 x max-x)
         (<= 0 y max-y))))

(def moves [[-1 0]
            [1 0]
            [0 1]
            [0 -1]])

(def cardinal-directions [:north
                          :south
                          :east
                          :west])

(def turns {:north #{:east :west}
            :south #{:east :west}
            :east #{:north :south}
            :west #{:north :south}})

(defn turn-from [current-direction]
  (let [valid (get turns current-direction)]
    (fn [new-direction]
      (contains? valid new-direction))))

(defn continue-in [current-direction]
  (fn [new-direction]
    (= current-direction new-direction)))

(def move->cardinal (zipmap moves cardinal-directions))
(def cardinal->move (set/map-invert move->cardinal))

(defn move [[x y] cardinal-direction]
  (let [[dx dy] (cardinal->move cardinal-direction)]
    [(+ x dx)
     (+ y dy)]))

(defn neighborhood [matrix coordinate]
  (let [candidates (map (partial move coordinate) cardinal-directions)
        chars (map (partial get-in matrix) candidates)]
    (filter (comp pathway? val) (zipmap cardinal-directions chars))))

(defn make-packet [entry]
  {:position entry
   :letters []
   :direction :south})

(defn next-move [matrix {position :position
                         direction :direction :as packet}]
  (let [hood (neighborhood matrix position)
        underfoot (get-in matrix position)]
    (if (corner? underfoot)
      (first (filter (comp (turn-from direction) first) hood))
      (first (filter (comp (continue-in direction) first) hood)))))

(defn move-in [packet cardinal-direction]
  (-> packet
      (update :position #(move % cardinal-direction))
      (assoc :direction cardinal-direction)))

(defn remember-path [packet char]
  (if (letter? char)
    (update packet :letters conj char)
    packet))

(defn move-packet [matrix packet]
  (if-let [[cardinal-direction underfoot] (next-move matrix packet)]
    (-> packet
        (move-in cardinal-direction)
        (remember-path underfoot))
    packet))

(defn unmoved? [{a :position} {b :position}]
  (= a b))

(defn run []
  (let [matrix (read-input)
        entry (entry-point matrix)]
    (loop [packet (make-packet entry)]
      (let [next-packet (move-packet matrix packet)]
        (if (unmoved? packet next-packet)
          (str/join (get packet :letters))
          (recur next-packet))))))
