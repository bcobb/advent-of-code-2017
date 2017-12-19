(ns advent.eighteen
  (:require [clojure.string :as str]))

(def integer-pattern #"[0-9]+")

(defn initial-state []
  {:played []
   :position 0
   :recovered []
   :registry {}})

(def identity-or-zero (fnil identity 0))

(defn registry-get [state key]
  (get-in state [:registry key] 0))

(defn move-forward [state]
  (update state :position inc))

(defn ensure-register [state register]
  (update-in state [:registry register] identity-or-zero))

(defn update-register [state register & args]
  (let [state' (ensure-register state register)]
    (apply update-in state' [:registry register] args)))

(defn argument [i x]
  (if (zero? i)
    (if (re-find integer-pattern x)
      (Integer/parseInt x)
      x)
    (fn [state]
      (if (re-find integer-pattern x)
        (Integer/parseInt x)
        (registry-get state x)))))

(defn line->instruction [line]
  (let [tokens (str/split line #" ")
        name (first tokens)
        args (map-indexed argument (rest tokens))]
    {:name name
     :args args}))

(defn i:snd [a]
  (fn [state]
    (let [frequency (registry-get state a)]
      (-> state
          move-forward
          (ensure-register a)
          (update :played conj frequency)))))

(defn i:set [a v]
  (fn [state]
    (-> state
        move-forward
        (update-register a (constantly (v state))))))

(defn i:add [a v]
  (fn [state]
    (-> state
        move-forward
        (update-register a + (v state)))))

(defn i:mul [a b]
  (fn [state]
    (let [other (b state)]
      (-> state
          move-forward
          (update-register a * other)))))

(defn i:mod [a b]
  (fn [state]
    (-> state
        move-forward
        (update-register a #(mod % (b state))))))

(defn i:rcv [a]
  (fn [{played :played :as state}]
    (let [val (registry-get state a)
          last-played (last played)]
      (if (not (zero? val))
        (-> state
            move-forward
            (update :recovered conj last-played))
        (move-forward state)))))

(defn i:jgz [a v]
  (fn [state]
    (let [val-a (registry-get state a)]
      (if (> val-a 0)
        (update state :position + (v state))
        (move-forward state)))))

(def instruction->fn {"snd" #'i:snd
                      "set" #'i:set
                      "add" #'i:add
                      "mul" #'i:mul
                      "mod" #'i:mod
                      "rcv" #'i:rcv
                      "jgz" #'i:jgz})

(defn read-input []
  (str/split (slurp "resources/eighteen.txt") #"\n"))

(defn parse-instruction [{name :name args :args}]
  (let [f (get instruction->fn name ::no-function)]
    (apply f args)))

(defn evaluate-program [lines]
  (let [instructions (mapv
                      (comp parse-instruction line->instruction)
                      lines)]
    (loop [state (initial-state)
           position (get state :position)
           instruction (nth instructions position)]
      (if (not-empty (get state :recovered))
        state
        (let [next-state (instruction state)
              next-position (get next-state :position)
              next-instruction (nth instructions next-position)]
          (recur next-state
                 next-position
                 next-instruction))))))
