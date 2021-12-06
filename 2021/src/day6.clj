(ns day6
  (:require [aoc-commons :refer [parse-long]]
            [clojure.string :as str]))

(defn- read-input []
  (map parse-long
       (-> "inputs/day6"
           slurp
           str/trim-newline
           (str/split #","))))

(defn- tick [generation]
  (map dec generation))

(defn- populate [generation]
  (concat (map #(if (neg? %) 6 %) generation)
          (repeat (count (filter neg? generation)) 8)))

(defn- wait [days generation]
  (if (= days 0)
    generation
    (recur (dec days) (->> generation tick populate))))

(defn- part-1 [input]
  (count (wait 80 input)))

(defn- solve [days input]
  (reduce (fn [generation _]
            (reduce (fn [new-gen [tick conut]]
                      (if (> tick 0)
                        (update new-gen (dec tick) (fnil + 0) conut)
                        (-> new-gen
                            (update 6 (fnil + 0) conut)
                            (update 8 (fnil + 0) conut))))
                    {} generation))
          (frequencies input)
          (range days)))

(defn- part-2 [input]
  (reduce + (vals (solve 256 input))))

(defn run [& _]
  (println "Day 6 - Lanternfish")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 372300
    (println "  part two:" (part-2 input)))) ; 1675781200288
