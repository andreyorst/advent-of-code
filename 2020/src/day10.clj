(ns day10
  (:require [clojure.string :as str]))

(defn- read-jolt-adapters [input]
  (->> input
       slurp
       str/split-lines
       (map #(Integer. %))
       (into (sorted-set))))

(defn- find-adapter [max-jolts adapters]
  (first (filter #(<= % max-jolts) adapters)))

(defn- connect-adapters [adapters]
  (loop [jolts 0
         [one-jolts three-jolts] [0 0]
         adapters adapters
         old 0]
    (if-let [adapter (find-adapter (+ jolts 3) adapters)]
      (recur (+ jolts adapter)
             (condp = (- adapter old)
               1 [(inc one-jolts) three-jolts]
               3 [one-jolts (inc three-jolts)]
               [one-jolts three-jolts])
             (disj adapters adapter)
             adapter)
      (* one-jolts (inc three-jolts)))))

(defn- combinations [x]
  (- (reduce * (repeat (- x 2) 2))
     (if (> x 4)
       (/ (* (inc (- x 4)) (- x 4)) 2)
       0)))

(defn- adapter-combinations [jolts]
  (->> (conj jolts 0)
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       (partition-by #(= 1 %))
       (filter (fn [x] (every? #(= % 1) x)))
       (map (comp combinations inc #(reduce + %)))
       (reduce *)))

(defn run [& _]
  (println "Day 10 - Adapter Array")
  (let [adapters (read-jolt-adapters "inputs/day10.txt")]
    (println "  part one:" (connect-adapters adapters)) ; 2400
    (println "  part two:" (adapter-combinations adapters)))) ; 338510590509056
