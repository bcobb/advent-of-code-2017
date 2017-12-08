(ns advent.three)

(def transitions {:seek-max-x :seek-max-y
                  :seek-max-y :seek-min-x
                  :seek-min-x :seek-min-y
                  :seek-min-y :seek-max-x})

(def strategy->relative-move {:seek-max-x [1 0]
                              :seek-max-y [0 1]
                              :seek-min-x [-1 0]
                              :seek-min-y [0 -1]})

(def strategy->comparison {:seek-max-x (fn [a b] (not= (get-in b [:max-x :x])
                                                       (get-in a [:max-x :x])))

                           :seek-max-y (fn [a b] (not= (get-in b [:max-y :y])
                                                       (get-in a [:max-y :y])))

                           :seek-min-x (fn [a b] (not= (get-in b [:min-x :x])
                                                       (get-in a [:min-x :x])))

                           :seek-min-y (fn [a b] (not= (get-in b [:min-y :y])
                                                       (get-in a [:min-y :y])))})

(defn known-points [state]
  (conj (get state :points)
        (get state :position)))

(defn extremities [state]
  (let [points (known-points state)
        by-x (sort-by :x points)
        by-y (sort-by :y points)]
    {:min-x (first by-x)
     :max-x (last by-x)
     :min-y (first by-y)
     :max-y (last by-y)}))

(defn state
  ([points position]
   (state points position :seek-max-x))
  ([points position strategy]
   {:points points
    :position position
    :strategy strategy}))

(defn point [x y label]
  {:x x
   :y y
   :label label})

(defn point-add [point x y]
  (merge
   point
   {:x (+ x (get point :x))
    :y (+ y (get point :y))}))

(defn point-with-label [point label]
  (assoc point :label label))

(defn initial-state []
  (state [(point 0 0 1)]
         (point 0 0 1)))

(defn with-position [state position]
  (-> state
      (assoc :position position)
      (update :points conj position)))

(defn next-position [state]
  (let [strategy (get state :strategy)
        [relative-x relative-y] (get strategy->relative-move strategy)
        current (get state :position)
        current-label (get current :label)]
    (point-with-label (point-add current relative-x relative-y) (inc current-label))))

(defn one-away [state]
  (with-position state (next-position state)))

(defn walk [state]
  (let [next (one-away state)
        strategy (get state :strategy)
        strategy-change-fn (get strategy->comparison strategy)
        strategy-change? (strategy-change-fn (extremities next)
                                             (extremities state))]
    (if strategy-change?
      (let [new-strategy (get transitions strategy)]
        (-> next
            (assoc :strategy new-strategy)
            (assoc :points (vals (extremities next)))))
      (assoc next :points (vals (extremities next))))))


(defn current-label [state]
  (get-in state [:position :label]))

(defn walk-board-up-to [n]
  (loop [state (initial-state)]
    (if (= (current-label state) n)
      state
      (recur (walk state)))))

(defn coordinates [point]
  (vector (get point :x) (get point :y)))

(defn taxicab-distance [a b]
  (let [[x1 x2] (coordinates a)
        [y1 y2] (coordinates b)]
    (+ (Math/abs (- x1 y1))
       (Math/abs (- x2 y2)))))

(defn radial-distance-to [n]
  (let [board (walk-board-up-to n)
        position (get board :position)]
    (taxicab-distance position {:x 0 :y 0})))
