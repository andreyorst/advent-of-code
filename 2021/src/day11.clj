(ns day11
  (:require [aoc-commons :refer [parse-long slurp-lines]]))

(defn- read-input []
  (->> "inputs/day11"
       slurp-lines
       (mapv (partial mapv parse-long))))

(defn- tick [rows]
  (mapv (partial mapv inc) rows))

(defn- find-flashing-cells [rows]
  (->> rows
       (keep-indexed
        (fn [i row]
          (keep-indexed
           (fn [j oct]
             (when (> oct 9)
               [i j]))
           row)))
       flatten
       (partition 2)
       (keep seq)))

(defn- safe-update [rows point f]
  (if (get-in rows point)
    (update-in rows point f)
    rows))

(defn- flash
  ([rows] (flash rows #{}))
  ([rows flashed]
   (reduce (fn [[rows flashed] [x y]]
             (if (not (flashed [x y]))
               [(-> rows
                    (safe-update [(inc x) y] inc)
                    (safe-update [(dec x) y] inc)
                    (safe-update [x (inc y)] inc)
                    (safe-update [x (dec y)] inc)
                    (safe-update [(inc x) (inc y)] inc)
                    (safe-update [(dec x) (inc y)] inc)
                    (safe-update [(inc x) (dec y)] inc)
                    (safe-update [(dec x) (dec y)] inc))
                (conj flashed [x y])]
               [rows flashed]))
           [rows flashed] (find-flashing-cells rows))))

(defn- to-zero [rows]
  (reduce (fn [rows [x y]] (assoc-in rows [x y] 0))
          rows (find-flashing-cells rows)))

(defn- step [rows]
  (loop [rows rows
         [rows* flashed] (flash (tick rows))]
    (if (not= rows rows*)
      (recur rows* (flash rows* flashed))
      [(to-zero rows*)
       (count (filter #(> % 9) (flatten rows*)))])))

(defn- part-1 [input]
  (second (reduce (fn [[rows flashed] _]
                    (let [[rows flashed*] (step rows)]
                      [rows (+ flashed flashed*)]))
                  [input 0] (range 100))))

(defn- part-2 [input]
  (reduce (fn [rows i]
            (let [[rows flashed] (step rows)]
              (if (= 100 flashed)
                (reduced i)
                rows)))
          input (rest (range))))

(defn run [& _]
  (println "Day 11 - Dumbo Octopus")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))            ; 1679
    (println "  part two:" (part-2 input))))          ; 519
