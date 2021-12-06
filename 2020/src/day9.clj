(ns day9
  (:require [clojure.string :as str]))

(defn- read-data [data]
  (->> data
       slurp
       str/split-lines
       (map #(BigInteger. %))))

(defn- find-sum [data x]
  (or (first (filter #(= x %) (map #(+ (first data) %) (rest data))))
      (and data (find-sum (next data) x))))

(defn- find-weakness [preamble-size data]
  (let [preamble (take preamble-size data)
        data (drop preamble-size data)]
    (loop [preamble preamble
           data data]
      (if (find-sum preamble (first data))
        (recur (conj (into [] (drop 1 preamble)) (first data))
               (drop 1 data))
        (first data)))))

(defn- crack-xmas [weakness data]
  (when data
    (let [[state range] (reduce (fn [[_ range] n]
                                  (let [sum (apply +' n range)]
                                    (cond (= sum weakness) (reduced [:found range])
                                          (> sum weakness) (reduced [:_ range])
                                          :else [:_ (conj range n)])))
                                [:_ []] data)]
      (if (= state :found)
        (+' (apply min range) (apply max range))
        (recur weakness (next data))))))

(defn run [& _]
  (println "Day 9 - Encoding Error")
  (let [data (read-data "inputs/day9.txt")
        weakness (find-weakness 25 data)]
    (println "  part one:" weakness)    ; 14144619
    (println "  part two:" (str (crack-xmas weakness data))))) ; 1766397
