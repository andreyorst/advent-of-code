(ns day1
  (:require [clojure.string :as str]))

(defn- read-report
  "Naively assuming that report contains only unique entries and
  accumulating those into `hash-set` for fast lookup.

  A possible workaround would be to use `frequences` function, and
  later check if there were more than one entry."
  [input]
  (->> input
       slurp
       str/split-lines
       (map #(Integer. %))
       (into #{})))

(defn- fix-report-two-nums
  "Repeatedly checks if the result of subtracting current item
  from `2020` is available in the set.

  Second optional parameter is used to search for combination of three
  numbers."
  ([report]
   (fix-report-two-nums report 0))
  ([report x]
   (when-let [f (first report)]
     (let [report (disj report f)]
       (if-let [s (report (- 2020 f x))]
         (* f s)
         (recur report x))))))

(defn- fix-report-three-nums [report]
  (when-let [f (first report)]
    (let [report (disj report f)]
      (if-let [res (fix-report-two-nums report f)]
        (* res f)
        (recur report)))))

(defn run [& _]
  (println "Day 1 - Report Repair")
  (let [report (read-report "inputs/day1.txt")]
    (println "  part one:" (fix-report-two-nums report))     ; 751776
    (println "  part two:" (fix-report-three-nums report)))) ; 42275090
