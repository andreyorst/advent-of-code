(ns day5
  (:require [aoc-commons :refer [parse-long]]
            [clojure.string :as str]))

(defn- parse-coordinates [line]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+)\s+->\s+(\d+),(\d+)" line)]
    [[(parse-long x1) (parse-long y1)]
     [(parse-long x2) (parse-long y2)]]))

(defn- read-input []
  (->> "inputs/day5"
       slurp
       str/split-lines
       (map parse-coordinates)))

(defn- keep-non-diagonal [coordinates]
  (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))) coordinates))

(defn- build-line [[[x1 y1] [x2 y2]]]
  (let [[x1 x2] (sort [x1 x2])
        [y1 y2] (sort [y1 y2])]
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))]
      [x y])))

(defn- mark-line [field line]
  (let [fr (frequencies line)]
    (merge-with (fnil + 1) field fr)))

(defn- mark-lines [field lines]
  (reduce mark-line field lines))

(defn- create-field [input]
  (->> input
       keep-non-diagonal
       (map build-line)
       (mark-lines {})))

(defn- render [size points]
  (let [field (vec (repeat 10 (vec (repeat 10 "."))))]
    (->> points
         (reduce (fn [field [[x y] val]]
                   (assoc-in field [x y] (str val)))
                 field)
         (apply map vector)
         (map str/join)
         (str/join "\n")
         println)))

(defn- part-1 [input]
  (->> (create-field input)
       vals
       (filter #(> % 1))
       count))

(defn- build-diagonale-line [[[x1 y1] [x2 y2]]]
  (when (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
    (let [xdir (if (> 0 (- x2 x1)) -1 1)
          ydir (if (> 0 (- y2 y1)) -1 1)]
      (loop [line [[x1 y1]]]
        (let [[x y] (last line)]
          (if (and (not= x x2) (not= y y2))
            (recur (conj line [(+ x xdir) (+ y ydir)]))
            line))))))

(defn- part-2 [input]
  (->> input
       (keep build-diagonale-line)
       (mark-lines (create-field input))
       vals
       (filter #(> % 1))
       count))

(defn run [& _]
  (println "Day 5 - Hydrothermal Venture")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 5280
    (println "  part two:" (part-2 input)))) ; 16716
