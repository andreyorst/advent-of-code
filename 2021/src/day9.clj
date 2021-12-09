(ns day9
  (:require [aoc-commons :refer [parse-long]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn read-input []
  (->> "inputs/day9"
       slurp
       str/trim
       str/split-lines
       (mapv #(mapv (fn [s] (parse-long (str s))) %))))

(defn transpose [m]
  (apply mapv vector m))

(defn find-minimum [row]
  (->> row
       (map-indexed #(and (< %2 (nth row (dec %1) 9))
                          (< %2 (nth row (inc %1) 9))
                          %1))
       (filter number?)
       (into [])))

(defn find-row-min-points [rows]
  (->> rows
       (mapv find-minimum)
       (map-indexed #(mapv (fn [e] [%1 e]) %2))
       (into [])))

(defn to-single-level [rows]
  (reduce (fn [all row] (concat all row)) [] rows))

(defn lowest-points [rows]
  (let [min-rows (->> rows
                      find-row-min-points
                      to-single-level
                      set)
        min-cols (->> rows
                      transpose
                      find-row-min-points
                      to-single-level
                      (map (fn [[x y]] [y x]))
                      set)]
    (into [] (set/intersection min-rows min-cols))))

(defn part-1 [input]
  (->> input
       lowest-points
       (map #(get-in input %))
       (map inc)
       (reduce +)))

(defn part-of-basin? [val lowest]
  (<= lowest val 8))

(defn find-basin [coords rows [x y] val]
  (when (part-of-basin? (get-in rows [x y] 10) val)
    (vswap! coords conj [x y])
    (doseq [coord [[(inc x) y]
                   [(dec x) y]
                   [x (inc y)]
                   [x (dec y)]]]
      (find-basin coords rows coord (inc val)))))

(defn find-basins [rows]
  (let [points (lowest-points rows)]
    (for [p points]
      (let [coords (volatile! #{})]
        (find-basin coords rows p (get-in rows p))
        @coords))))

(defn part-2 [input]
  (->> input
       find-basins
       (map count)
       (sort >)
       (take 3)
       (reduce *)))

(defn run [& _]
  (println "Day 9 - Smoke Basin")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 518
    (println "  part two:" (part-2 input)))) ; 949905
