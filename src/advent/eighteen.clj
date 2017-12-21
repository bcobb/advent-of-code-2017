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
  (if (string? key)
    (get-in state [:registry key] 0)
    key))

(defn move-forward [state]
  (update state :position inc))

(defn ensure-register [state register]
  (update-in state [:registry register] identity-or-zero))

(defn update-register [state register & args]
  (let [state' (ensure-register state register)]
    (apply update-in state' [:registry register] args)))

(defn read-inbox-once [state]
  (update state :inbox (comp vec rest)))

(defn add-to-outbox [state message]
  (update state :outbox conj message))

(defn increment-sent [state]
  (update state :sent-messages inc))

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
  (fn [state]
    (let [inbox (get state :inbox)]
      (if (empty? inbox)
        (assoc state :state :waiting)
        (let [message (first inbox)]
          (-> state
              (assoc :state :running)
              move-forward
              read-inbox-once
              (update-register a (constantly message))))))))

(defn i:snd [a]
  (fn [state]
    (let [message (registry-get state a)]
      (-> state
          move-forward
          increment-sent
          (add-to-outbox message)))))

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

(defn lines->instructions [lines]
  (mapv
   (comp parse-instruction line->instruction)
   lines))

(defn evaluate-program [lines]
  (let [instructions (lines->instructions lines)]
    (loop [state (initial-state)
           position (get state :position)
           instruction (nth instructions position)]
      (if (nil? instruction)
        state
        (let [next-state (instruction state)
              next-position (get next-state :position)
              next-instruction (nth instructions next-position)]
          (recur next-state
                 next-position
                 next-instruction))))))

(defn make-program [number]
  {:id number
   :position 0
   :registry {"p" number}
   :state :running
   :sent-messages 0
   :inbox []
   :outbox []})

(defn evaluation-context [instructions]
  {:instructions instructions
   :programs {0 (make-program 0)
              1 (make-program 1)}})

(defn dispatch-message [context sender-entry]
  (let [sender-id (key sender-entry)
        sender (val sender-entry)
        messages (get sender :outbox)
        programs (get context :programs)
        recipient (-> programs
                       (dissoc sender-id)
                       vals
                       first)
        recipient-id (get recipient :id)]
    (-> context
        (assoc-in [:programs sender-id :outbox] [])
        (update-in [:programs recipient-id :inbox] into messages))))

(defn dispatch-messages [context]
  (let [programs (get context :programs)]
    (reduce dispatch-message context programs)))

(defn evaluate-next-instruction [context program-entry]
  (let [program-id (key program-entry)
        program (val program-entry)
        position (get program :position)
        instructions (get context :instructions)
        instruction (nth instructions position)]
    (-> context
        (update-in [:programs program-id] instruction)
        dispatch-messages)))

(defn deadlocked? [context]
  (let [all-waiting? (every? (partial = :waiting) (map (comp :state val) (get context :programs)))
        inboxes-empty? (every? empty? (map (comp :inbox val) (get context :programs)))]
    (and all-waiting? inboxes-empty?)))

(defn run-again []
  (let [instructions (lines->instructions (read-input))
        initial-context (evaluation-context instructions)]
    (loop [context initial-context]
      (if (deadlocked? context)
        (get-in context [:programs 1 :sent-messages])
        (let [programs (get context :programs)
              new-context (reduce evaluate-next-instruction context programs)]
          (recur new-context))))))
