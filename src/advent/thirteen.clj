(ns advent.thirteen
  (:require [clojure.string :as str]))

(defn update-firewall [firewall line]
  (let [[depth range] (map #(Integer/parseInt %) (str/split line #": "))]
    (assoc firewall depth range)))

(defn max-depth [firewall]
  (apply max (keys firewall)))

(defn min-depth [firewall]
  (apply min (keys firewall)))

(defn depths [firewall]
  (range (min-depth firewall)
         (inc (max-depth firewall))))

(defn range-at [firewall depth]
  (get firewall depth 0))

(defn oscillate-to [n]
  (if (zero? n)
    (repeat 0)
    (letfn [(oscillator
              ([] (oscillator 0 1))
              ([a b]
               (let [step (if (or (< a b n) (zero? b)) inc dec)]
                 (lazy-seq (cons a (oscillator b (step b)))))))]
      (oscillator))))

(defn scanner-position [firewall depth t]
  (let [max-index (dec (range-at firewall depth))
        oscillator (oscillate-to max-index)]
    (last (take (inc t) oscillator))))

(defn caught-at? [firewall depth t]
  (when (get firewall depth)
    (zero? (scanner-position firewall depth t))))

(defn severity [firewall depth t]
  (if (caught-at? firewall depth t)
    (* depth (range-at firewall depth))
    0))

(defn parse-input [input]
  (let [lines (str/split input #"\n")
        firewall (reduce update-firewall {} lines)]
    firewall))

(defn get-firewall []
  (parse-input (slurp "resources/thirteen.txt")))

(defn run []
  (let [firewall (get-firewall)]
    (apply + (map (partial severity firewall)
                  (depths firewall)
                  (depths firewall)))))

(defn caught-at? [layer range delay]
  (let [dividend (* 2 (dec range))
        packet-position (+ layer delay)]
    (zero? (mod packet-position dividend))))

(defn position-equations [firewall]
  (map (fn [me]
         (let [layer (key me)
               range (val me)]
           (partial caught-at? layer range)))
       firewall))

(defn evades-capture? [equations delay]
  (empty?
   (filter identity
           (map #(% delay) equations))))

(defn run-again []
  (let [firewall (get-firewall)
        eqs (position-equations firewall)]
    (first (filter (partial evades-capture? eqs)
                   (range)))))
