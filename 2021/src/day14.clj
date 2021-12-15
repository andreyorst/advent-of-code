(ns day14
  (:require [clojure.string :as str]
            [aoc-commons :refer [slurp-lines]]))

(defn- parse-recipe [recipe]
  (let [[pattern replacement]
        (str/split recipe #"\s+->\s+")]
    [(seq pattern) (first replacement)]))

(defn- read-input []
  (let [lines (slurp-lines "inputs/day14")
        polymer (first lines)
        recipes (drop 2 lines)]
    {:polymer (seq polymer)
     :recipes (into {} (map parse-recipe) recipes)}))

(defn- solve [n {:keys [polymer recipes]}]
  (loop [n n
         letters (frequencies polymer)
         pairs (frequencies (partition 2 1 polymer))]
    (if (> n 0)
      (let [[letters pairs]
            (reduce (fn [[letters pairs]
                         [[a b :as pair] n]]
                      (let [letter (recipes pair)
                            add (fnil (partial + n) 0)]
                        [(update letters letter add)
                         (-> pairs
                             (update [a letter] add)
                             (update [letter b] add))]))
                    [letters {}]
                    pairs)]
        (recur (dec n) letters pairs))
      (- (apply max (vals letters))
         (apply min (vals letters))))))

(defn run [& _]
  (println "Day 14 - Extended Polymerization")
  (let [input (read-input)]
    (println "  part one:" (solve 10 input))   ; 2447
    (println "  part two:" (solve 40 input)))) ; 3018019237563
