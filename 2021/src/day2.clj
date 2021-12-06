(ns day2
  (:require [clojure.string :as str]
            [aoc-commons :refer [parse-long]]))

(defn- read-input []
  (->> "inputs/day2"
       slurp
       str/split-lines
       (map #(str/split % #"\s+"))
       (map #(update % 1 parse-long))))

(defmulti ^:private move
  "Dispatching on `direction`. Returning the result applicable by
  `reduce`."
  (fn [_ [direction _]] direction))

(defmethod move "forward" [[x y] [_ amount]] [(+ x amount) y])
(defmethod move "up" [[x y] [_ amount]] [x (- y amount)])
(defmethod move "down" [[x y] [_ amount]] [x (+ y amount)])

(defn- part-1 [input]
  (->> input
       (reduce move [0 0])
       (apply *)))

(defn- move-and-aim
  "`case` based version of move function.
  Does essentially the same thing as `move`, just works accordingly to
  part 2 rules, and written in a different style."
  [[x y aim] [direction amount]]
  (case direction
    "up" [x y (- aim amount)]
    "down" [x y (+ aim amount)]
    "forward" [(+ x amount) (+ y (* aim amount)) aim]))

(defn- part-2 [input]
  (->> input
       (reduce move-and-aim [0 0 0])
       (take 2)
       (apply *)))

(defn run [& _]
  (println "Day 2 - Dive!")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 2036120
    (println "  part two:" (part-2 input)))) ; 2015547716
