(ns day25
  (:require [aoc-commons :refer [slurp-lines transpose]]))

(defn- read-input []
  (->> "inputs/day25"
       slurp-lines
       (mapv vec)))

(defn- move [kind row]
  (let [len (count row)]
    (reduce-kv
     (fn [row' i c]
       (if (= c kind)
         (let [pos (mod (inc i) len)]
           (if (= (nth row pos) \.)
             (assoc row' i \. pos kind)
             row'))
         row'))
     row row)))

(defn- step [world]
  (->> world
       (mapv (partial move \>))
       transpose
       (mapv (partial move \v))
       transpose))

(defn- part-1 [input]
  (reduce (fn [world i]
            (let [world' (step world)]
              (if (= world' world)
                (reduced i)
                world')))
          input (drop 1 (range))))

(defn run [& _]
  (println "Day 25 - Sea Cucumber")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))
    #_(println "  part two:" (part-2 input))))
