(ns day1
  (:require [clojure.string :as str]
            [aoc-commons :refer [parse-long]]))

(defn- read-input []
  (->> "inputs/day1"
       slurp
       str/split-lines
       (map parse-long)))

(defn- part-1 [input]
  (->> input
       (partition 2 1)
       (filter (partial apply <))
       count))

(defn- part-2 [input]
  (->> input
       (partition 3 1)
       (map (partial apply +))
       part-1))

(defn run [& _]
  (println "Day 1 - Sonar Sweep")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 1215
    (println "  part two:" (part-2 input)))) ; 1150
