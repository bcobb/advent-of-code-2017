(ns advent.sixteen
  (:require [clojure.string :as str]))

(defn to-i [s]
  (Integer/parseInt s))

(defn make-spin [x]
  (fn [programs]
    (let [total (count programs)
          start-index (- total x)
          end (subvec programs start-index)
          beginning (subvec programs 0 start-index)]
      (into end beginning))))

(defn make-exchange [a b]
  (fn [programs]
    (let [source (nth programs a)
          dest (nth programs b)]
      (-> programs
          (assoc b source)
          (assoc a dest)))))

(defn index-of [coll val]
  (ffirst (filter
          #(= (last %) val)
          (map-indexed vector coll))))

(defn make-partner [a b]
  (fn [programs]
    (let [source (index-of programs a)
          dest (index-of programs b)]
      (-> programs
          (assoc source b)
          (assoc dest a)))))

(defn dance [programs moves]
  (reduce (fn [programs' move] (move programs'))
          programs
          moves))

(defmulti parse-move first)

(defmethod parse-move \x parse-exchange [move]
  (let [params (str/split
                (str/join (rest move))
                #"/")]
    (apply make-exchange (map to-i params))))

(defmethod parse-move \s parse-spin [move]
  (let [params (vector (str/join (rest move)))]
    (apply make-spin (map to-i params))))

(defmethod parse-move \p parse-partner [move]
  (let [params (str/join (str/split
                          (str/join (rest move))
                          #"/"))]
    (apply make-partner (vec params))))

(defn parse-moves [doc]
  (let [raw-moves (str/split doc #",")]
    (map parse-move raw-moves)))

(defn read-input []
  (slurp "resources/sixteen.txt"))

(defn alphabet-range [start end]
  (let [start-i (int start)
        end-i (inc (int end))]
    (map char (range start-i end-i))))

(defn run []
  (dance (vec (alphabet-range \a \p))
         (parse-moves (read-input))))


(defn moves-until-cycle [initial-programs moves]
  (loop [programs initial-programs
         i 0]
    (if (and (= programs initial-programs)
             (not (zero? i)))
      i
      (recur (dance programs moves)
             (inc i)))))

(defn dance-n-times [n initial-programs moves]
  (loop [programs initial-programs
         i 0]
    (if (= i n)
      programs
      (recur (dance programs moves)
             (inc i)))))

(defn run-again []
  (let [programs (vec (alphabet-range \a \p))
        moves (parse-moves (read-input))
        cycle-length (moves-until-cycle programs moves)
        one-billion 1000000000
        effective-moves (mod one-billion cycle-length)]
    (dance-n-times effective-moves programs moves)))
