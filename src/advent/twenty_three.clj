(ns advent.twenty-three
  (:require [clojure.string :as str]))

(defn program-state [instructions]
  {:instructions instructions
   :position 0
   :invocations {}
   :registry {}})

(defn track-invocation [state instruction]
  (update-in state [:invocations instruction] (fnil inc 0)))

(defn program-lines []
  (str/split (slurp "resources/twenty_three.txt") #"\n"))

(defn advance-position [state]
  (update state :position inc))

(defn i:set [key-fn val-fn]
  (fn [{:keys [registry] :as state}]
    (-> state
        advance-position
        (assoc-in [:registry (key-fn)] (val-fn registry))
        (track-invocation "set"))))

(defn i:sub [key-fn val-fn]
  (fn [{:keys [registry] :as state}]
    (let [register (key-fn)
          current (get registry register 0)
          negation (val-fn registry)]
      (-> state
          advance-position
          (assoc-in [:registry register] (- current negation))
          (track-invocation "sub")))))

(defn i:mul [key-fn val-fn]
  (fn [{:keys [registry] :as state}]
    (let [register (key-fn)
          current (get registry register 0)
          multiplier (val-fn registry)]
      (-> state
          advance-position
          (assoc-in [:registry register] (* current multiplier))
          (track-invocation "mul")))))

(defn jnz-guard [registry key-fn]
  (let [maybe-register (key-fn)]
    (if (int? maybe-register)
      maybe-register
      (get registry maybe-register 0))))

(defn i:jnz [key-fn val-fn]
  (fn [{:keys [registry] :as state}]
    (let [guard (jnz-guard registry key-fn)]
      (if (zero? guard)
        (-> state
            advance-position
            (track-invocation "jnz"))
        (-> state
            (update :position + (val-fn registry))
            (track-invocation "jnz"))))))

(def instruction->fn {"set" #'i:set
                      "sub" #'i:sub
                      "mul" #'i:mul
                      "jnz" #'i:jnz})

(def numeric-pattern #"[0-9-]+")

(defmulti parse-argument (fn [position _] position))

(defmethod parse-argument 0 parse-first-argument [_ argument]
  (if (re-find numeric-pattern argument)
    (constantly (Integer/parseInt argument))
    (constantly argument)))

(defmethod parse-argument 1 parse-second-argument [_ argument]
  (if (re-find numeric-pattern argument)
    (constantly (Integer/parseInt argument))
    (fn [registry] (get registry argument 0))))

(defn parse-line [line]
  (let [[instruction & arguments] (str/split line #" ")
        instruction-fn (get instruction->fn instruction)]
    (apply instruction-fn (map-indexed parse-argument arguments))))

(defn parse-lines [lines]
  (mapv parse-line lines))

(defn run-program []
  (let [lines (program-lines)
        instructions (parse-lines lines)]
    (loop [{:keys [position] :as state} (program-state instructions)]
      (let [instruction (get-in state [:instructions position])]
        (if (nil? instruction)
          state
          (recur (instruction state)))))))

(defn run []
  (get (run-program) :invocations))
