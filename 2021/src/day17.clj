(ns day17
  (:require [aoc-commons :refer [parse-long]]))

(defn- read-input []
  (let [s (slurp "inputs/day17")
        vals (drop 1 (re-find #"x=(-?\d+)\.\.(-?\d+),\s+y=(-?\d+)\.\.(-?\d+)$" s))]
    (map parse-long vals)))

(defn- triangle-number [n]
  (/ (* n (inc n)) 2))

(defn- part-1 [input]
  (let [[_ _ min-y] input]
    (triangle-number min-y)))

(defn- update-x-vel [vel]
  (cond (< vel 0) (inc vel)
        (> vel 0) (dec vel)
        :else 0))

(defn- solve
  ([x-vel y-vel min-x max-x min-y max-y]
   (when (solve 0 x-vel 0 y-vel min-x max-x min-y max-y)
     [x-vel y-vel]))
  ([x x-vel y y-vel min-x max-x min-y max-y]
   (cond (and (<= min-x x max-x) (<= min-y y max-y))
         true
         (or (and (< y min-y) (< y-vel 0))
             (> x max-x)
             (and (< x min-x) (= x-vel 0)))
         nil
         :else
         (recur (+ x x-vel) (update-x-vel x-vel)
                (+ y y-vel) (dec y-vel)
                min-x max-x
                min-y max-y))))

(defn- solve-all [[min-x max-x min-y max-y]]
  (->> (for [xv (range (inc max-x))
             yv (range (- (Math/abs min-y)) (Math/abs min-y))]
         (solve xv yv min-x max-x min-y max-y))
       (filter some?)
       distinct))

(defn- part-2 [input]
  (->> (solve-all input)
       distinct
       count))

(defn run [& _]
  (println "Day 17 - Trick Shot")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 15931
    (println "  part two:" (part-2 input)))) ; 2555
