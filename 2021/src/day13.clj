(ns day13
  (:require [clojure.string :as str]
            [aoc-commons :refer [parse-long slurp-lines transpose]]))

(defn- read-input []
  (let [lines (slurp-lines "inputs/day13")
        points (take-while seq lines)
        folds (rest (drop-while seq lines))]
    {:points (mapv #(mapv parse-long (str/split % #",")) points)
     :folds (mapv #(update (-> (str/replace % #"fold along\s+" "")
                               (str/split #"="))
                           1
                           parse-long)
                  folds)}))

(defn- fold [point along coord]
  (case along
    "x" (let [[x y] point]
          (if (> x coord)
            [(- coord (- x coord)) y]
            [x y]))
    "y" (let [[x y] point]
          (if (> y coord)
            [x (- coord (- y coord))]
            [x y]))))

(defn- render [points]
  (let [x (inc (apply max (map first points)))
        y (inc (apply max (map second points)))
        field (into [] (repeat x (into [] (repeat y "."))))]
    (->> points
         (reduce (fn [field p] (assoc-in field p "#")) field)
         transpose
         (map str/join)
         (map (partial str "  "))
         (str/join "\n")
         println)))

(defn- part-1 [input]
  (let [{:keys [points folds]} input
        [along coord] (first folds)]
    (->> points
         (mapv #(fold % along coord))
         distinct
         count)))

(defn- part-2 [input]
  (let [{:keys [points folds]} input]
    (->> folds
         (reduce (fn [paper [along coord]]
                   (mapv #(fold % along coord) paper)) points)
         render)))

(defn run [& _]
  (println "Day 13 - Transparent Origami")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 631
    (println "  part two:") (part-2 input))) ; EFLFJGRF
