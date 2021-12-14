(ns day3
  (:require [aoc-commons :refer [parse-long slurp-lines]]
            [clojure.string :as str]))

(defn- read-input []
  (->> "inputs/day3"
       slurp-lines
       (map seq)
       (map #(mapv (fn [c] (parse-long (str c))) %))))

(defn- count-bits [bit-n input]
  (->> input
       (map #(get % bit-n))
       sort
       (split-with (partial = 0))
       (map count)
       (zipmap [:ones :zeroes])))

(defn- flip-bit [b]
  (if (= b 0) 1 0))

(defn- most-common-bits [input]
  (->> input
       first
       count
       range
       (map #(count-bits % input))
       (map (fn [{:keys [zeroes ones]}]
              (if (> zeroes ones) 1 0)))))

(defn- part-1 [input]
  (let [gamma (most-common-bits input)
        epsilon (map flip-bit gamma)]
    (* (Long/parseLong (str/join gamma) 2)
       (Long/parseLong (str/join epsilon) 2))))

(defn- bit-criteria [type {:keys [ones zeroes]}]
  (case type
    :oxygen (if (< zeroes ones) 0 1)
    :CO2 (if (< zeroes ones) 1 0)))

(defn- calculate [type input]
  (let [total (count (first input))]
    (loop [n 0
           numbers input]
      (let [bits (count-bits n numbers)
            criteria (bit-criteria type bits)
            res (filter #(= (get % n) criteria) numbers)]
        (if (= 1 (count res))
          (first res)
          (when (< n total)
            (recur (inc n) res)))))))

(defn- part-2 [input]
  (let [oxygen (calculate :oxygen input)
        co2 (calculate :CO2 input)]
    (* (Long/parseLong (str/join oxygen) 2)
       (Long/parseLong (str/join co2) 2))))

(defn run [& _]
  (println "Day 3 - Binary Diagnostic")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 3885894
    (println "  part two:" (part-2 input)))) ; 4375225
