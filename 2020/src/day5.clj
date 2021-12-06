(ns day5
  (:require [clojure.string :as str]))

(defn- read-boarding-list [boarding]
  (->> boarding
       slurp
       str/split-lines))

(defn- average [col]
  (int (float (/ (apply + col) (count col)))))

(defn- calc-id [scheme]
  (loop [scheme scheme
         row [1 128]
         col [1 8]]
    (if (seq scheme)
      (recur (rest scheme)
             (condp = (first scheme)
               \F [(row 0) (average row)]
               \B [(average row) (row 1)]
               row)
             (condp = (first scheme)
               \L [(col 0) (average col)]
               \R [(average col) (col 1)]
               col))
      (+ (* (dec (row 1))
            8)
         (dec (col 1))))))

(defn- my-seat [ids]
  (let [seats (sort ids)
        low (apply min seats)]
    (reduce (fn [current next]
              (if (not= (inc current) next)
                (reduced (inc current))
                next))
            low
            (drop 1 seats))))

(defn run [& _]
  (println "Day 5 - Binary Boarding")
  (let [boardings (read-boarding-list "inputs/day5.txt")
        seat-ids (map calc-id boardings)]
    (println "  part one:" (apply max seat-ids))  ; 908
    (println "  part two: " (my-seat seat-ids)))) ; 619
