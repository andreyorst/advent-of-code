(ns day3
  (:require [clojure.string :as str]))

(defn- read-tree-map [map]
  (let [lines (->> map
                   slurp
                   str/split-lines)]
    {:width (-> lines first count)
     :rows (mapv #(into [] %) lines)}))

(defn- traverse [right down map]
  (loop [rows (drop down (:rows map))
         pos 0
         trees 0]
    (if-let [line (first rows)]
      (let [new-pos (mod (+ pos right) (:width map))]
        (recur (drop down rows)
               new-pos
               (if (= (line new-pos) \#) (inc trees) trees)))
      trees)))

(defn run [& _]
  (println "Day 3 - Toboggan Trajectory")
  (let [map-data (read-tree-map "inputs/day3.txt")]
    (println "  part one:" (traverse 3 1 map-data)) ; 237
    (println "  part two:" (* (traverse 1 1 map-data)
                              (traverse 3 1 map-data)
                              (traverse 5 1 map-data)
                              (traverse 7 1 map-data)
                              (traverse 1 2 map-data))))) ; 2106818610
