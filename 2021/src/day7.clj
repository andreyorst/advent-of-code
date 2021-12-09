(ns day7
  (:require [aoc-commons :refer [parse-long]]
            [clojure.string :as str]))

(defn- read-input []
  (map parse-long
       (-> "inputs/day7"
           slurp
           str/trim-newline
           (str/split #","))))

(defn- fuel-consumption [input modifier pos]
  (->> input
       (map #(modifier (Math/abs (- % pos))))
       (reduce +)))

(defn- solve
  ([input] (solve input identity))
  ([input modifier]
   (let [min (apply min input)
         max (apply max input)]
     (->> (range min (inc max))
          (map #(fuel-consumption input modifier %))
          sort
          first))))

(defn- triangle-number [n]
  (/ (* n (inc n)) 2))

(defn- part-1 [input]
  (solve input))

(defn- part-2 [input]
  (solve input triangle-number))

(defn run [& _]
  (println "Day 7 - The Treachery of Whales")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 343468
    (println "  part two:" (part-2 input)))) ; 96086265
