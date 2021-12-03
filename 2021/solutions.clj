(ns aoc-commons)

(defn parse-long [s]
  (if (string? s)
    (try
      (Long/valueOf s)
      (catch NumberFormatException _
        nil))
    (throw (IllegalArgumentException.
            (format "Expected string, got %s" (some-> s class .getName))))))


(ns day1
  (:require [clojure.string :as str]
            [aoc-commons :refer [parse-long]]))

(defn- read-input []
  (->> "inputs/day1"
       slurp
       str/split-lines
       (map parse-long)))

(defn- part-1 [input]
  (->> input
       (partition 2 1)
       (filter (partial apply <))
       count))

(defn- part-2 [input]
  (->> input
       (partition 3 1)
       (map (partial apply +))
       part-1))

(defn sonar-sweep []
  (println "Day 1 - Sonar Sweep")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 1215
    (println "  part two:" (part-2 input)))) ; 1150


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

(defn dive! []
  (println "Day 2 - Dive!")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 2036120
    (println "  part two:" (part-2 input)))) ; 2015547716


(ns day3
  (:require [aoc-commons :refer [parse-long]]
            [clojure.string :as str]))

(defn- read-input []
  (->> "inputs/day3"
       slurp
       str/split-lines
       (map seq)
       (map #(mapv (fn [c] (parse-long (str c))) %))))

(defn- count-bits [bit-n input]
  (->> input
       (map #(get % bit-n))
       sort
       (split-with (partial = 0))
       (map count)
       (zipmap [:ones :zeroes])))


(defn- flip-bit [b]
  (if (= b 0) 1 0))

(defn- most-common-bits [input]
  (->> input
       first
       count
       range
       (map #(count-bits % input))
       (map (fn [{:keys [zeroes ones]}]
              (if (> zeroes ones) 1 0)))))

(defn- part-1 [input]
  (let [gamma (most-common-bits input)
        epsilon (map flip-bit gamma)]
    (* (Long/parseLong (str/join gamma) 2)
       (Long/parseLong (str/join epsilon) 2))))

(defn- bit-criteria [type {:keys [ones zeroes]}]
  (case type
    :oxygen (if (< zeroes ones) 0 1)
    :CO2 (if (< zeroes ones) 1 0)))

(defn- calculate [type input]
  (let [total (count (first input))]
    (loop [n 0
           numbers input]
      (let [bits (count-bits n numbers)
            criteria (bit-criteria type bits)
            res (filter #(= (get % n) criteria) numbers)]
        (if (= 1 (count res))
          (first res)
          (when (< n total)
            (recur (inc n) res)))))))

(defn- part-2 [input]
  (let [oxygen (calculate :oxygen input)
        co2 (calculate :CO2 input)]
    (* (Long/parseLong (str/join oxygen) 2)
       (Long/parseLong (str/join co2) 2))))

(defn binary-diagnostic []
  (println "Binary Diagnostic")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 3885894
    (println "  part two:" (part-2 input)))) ; 4375225


(ns aoc2021
  (:require day1 day2 day3))

(day1/sonar-sweep)
(day2/dive!)
(day3/binary-diagnostic)
