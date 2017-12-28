(ns advent.twenty-five)

(def blueprints {:A {0 {:write 1
                        :move :right
                        :next :B}
                     1 {:write 0
                        :move :left
                        :next :C}}
                 :B {0 {:write 1
                        :move :left
                        :next :A}
                     1 {:write 1
                        :move :right
                        :next :C}}
                 :C {0 {:write 1
                        :move :right
                        :next :A}
                     1 {:write 0
                        :move :left
                        :next :D}}
                 :D {0 {:write 1
                        :move :left
                        :next :E}
                     1 {:write 1
                        :move :left
                        :next :C}}
                 :E {0 {:write 1
                        :move :right
                        :next :F}
                     1 {:write 1
                        :move :right
                        :next :A}}
                 :F {0 {:write 1
                        :move :right
                        :next :A}
                     1 {:write 1
                        :move :right
                        :next :E}}})

(defn initial-machine [initial-state]
  {:state initial-state
   :position 0
   :on #{}})

(defmulti write (fn [_ value] value))

(defmethod write 0 [{:keys [position] :as state} value]
  (update state :on disj position))

(defmethod write 1 [{:keys [position] :as state} value]
  (update state :on conj position))

(defmulti move (fn [_ value] value))

(defmethod move :left [{:keys [position] :as state} value]
  (update state :position dec))

(defmethod move :right [{:keys [position] :as state} value]
  (update state :position inc))

(defn current-value [{:keys [on position]}]
  (if (contains? on position)
    1
    0))

(defn move-once [{:keys [state] :as machine}]
  (let [value (current-value machine)
        {to-write :write
         to-move :move
         next-state :next} (get-in blueprints [state value])]
    (-> machine
        (write to-write)
        (move to-move)
        (assoc :state next-state))))

(defn diagnostic-checksum [{:keys [on]}]
  (count on))

(defn run []
  (let [steps-needed 12134527]
    (loop [machine (initial-machine :A)
           steps-taken 0]
      (if (= steps-taken steps-needed)
        (diagnostic-checksum machine)
        (recur (move-once machine)
               (inc steps-taken))))))
