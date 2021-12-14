(ns day4
  (:require [aoc-commons :refer [parse-long slurp-lines]]
            [clojure.string :as str]))

(defn- parse-draws [lines]
  (map parse-long (str/split lines #",")))

(defn- prepare-row [row]
  (mapv parse-long
        (-> row
            str/trim
            (str/split #"\s+"))))

(defn- parse-boards [lines]
  (->> lines
       (remove empty?)
       (partition 5)
       (mapv (partial mapv prepare-row))))

(defn- read-input []
  (let [lines (slurp-lines "inputs/day4")]
    {:draws (parse-draws (first lines))
     :boards (parse-boards (drop 1 lines))}))

(defn- mark-board [number board]
  (mapv #(mapv (fn [x] (if (= number x) nil x)) %) board))

(defn- rotate [board]
  (apply mapv vector board))

(defn- check-board [board]
  (when (or (some (partial every? nil?) board)
            (some (partial every? nil?) (rotate board)))
    board))

(defn- find-winning-board [draws boards]
  (loop [[number & draws] draws
         boards boards]
    (let [boards (mapv (partial mark-board number) boards)]
      (if-let [winning-board (some check-board boards)]
        {:win-number number
         :board winning-board}
        (recur draws
               boards)))))

(defn- calculate-result [{:keys [win-number board]}]
  (* win-number
     (->> board
          flatten
          (filter number?)
          (apply +))))

(defn- part-1 [{:keys [draws boards]}]
  (->> boards
       (find-winning-board draws)
       calculate-result))

(defn- find-last-winning-board [draws boards]
  (loop [[number & draws] draws
         boards boards]
    (let [boards (mapv (partial mark-board number) boards)
          non-winning (remove check-board boards)]
      (if (empty? non-winning)
        {:win-number number
         :board (last boards)}
        (recur draws
               non-winning)))))

(defn- part-2 [{:keys [draws boards]}]
  (->> boards
       (find-last-winning-board draws)
       calculate-result))

(defn run [& _]
  (println "Day 4 - Giant Squid")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 29440
    (println "  part two:" (part-2 input)))) ; 13884
