(ns day15
  (:require [aoc-commons :refer [parse-long slurp-lines]]))

(defn- read-input []
  (->> "inputs/day15"
       slurp-lines
       (map seq)
       (mapv #(mapv parse-long %))))

(defn- dijkstra [world end]
  (loop [points (sorted-set [0 [0 0]])
         visited #{}]
    (let [[total [x y] :as point] (first points)]
      (if (= [x y] end)
        total
        (let [visited (conj visited [x y])
              points (reduce
                      (fn [paths [new-x new-y]]
                        (if-let [risk (and (not (visited [new-x new-y]))
                                           (get-in world [new-x new-y]))]
                          (conj paths [(+ total risk) [new-x new-y]])
                          paths))
                      (disj points point)
                      [[(dec x) y] [(inc x) y]
                       [x (inc y)] [x (dec y)]])]
          (recur points visited))))))

(defn- part-1 [input]
  (let [end-x (dec (count (first input)))
        end-y (dec (count input))]
    (dijkstra input [end-x end-y])))

(defn- add-to-field [field x]
  (let [add-to-row
        (fn [row]
          (mapv #(let [x (+ % x)]
                   (if (> x 9)
                     (- x 9)
                     x))
                row))]
    (mapv add-to-row field)))

(defn- extend-field [input]
  (let [new-world (reduce (fn [world n]
                            (let [new-world (add-to-field input n)]
                              (mapv #(into %1 %2) world new-world)))
                          input (range 1 5))]
    (reduce (fn [world n]
              (let [new-world (add-to-field new-world n)]
                (into world new-world)))
            new-world (range 1 5))))

(defn- part-2 [input]
  (let [world (extend-field input)
        end-x (dec (count (first world)))
        end-y (dec (count world))]
    (dijkstra world [end-x end-y])))

(defn run [& _]
  (println "Day 15 - Chiton")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))
    (println "  part two:" (part-2 input))))
