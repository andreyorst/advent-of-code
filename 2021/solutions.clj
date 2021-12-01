(ns day1
  (:require [clojure.string :as str]))

(defn- read-input []
  (->> "inputs/day1"
       slurp
       str/split-lines
       (map #(Integer/parseInt %))))

(defn- part-1 [input]
  (->> input
       (partition 2 1)
       (reduce (fn [res [current next]]
                 (+ res (if (> next current) 1 0)))
               0)))

(defn- part-2 [input]
  (->> input
       (partition 3 1)
       (map (fn [nums] (apply + nums)))
       part-1))

(defn sonar-sweep []
  (println "Day 1 - Sonar Sweep")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 1215
    (println "  part two:" (part-2 input)))) ; 1150


(ns aoc2021
  (:require [day1]))

(day1/sonar-sweep)
