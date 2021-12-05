(ns aoc-commons
  (:require [clojure.string :as str]))

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


(ns day4
  (:require [aoc-commons :refer [parse-long]]
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
  (let [lines (->> "inputs/day4"
                   slurp
                   str/split-lines)]
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

(defn giant-squid []
  (println "Giant Squid")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 29440
    (println "  part two:" (part-2 input)))) ; 13884


(ns day5
  (:require [aoc-commons :refer [parse-long]]
            [clojure.string :as str]))

(defn- parse-coordinates [line]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+)\s+->\s+(\d+),(\d+)" line)]
    [[(parse-long x1) (parse-long y1)]
     [(parse-long x2) (parse-long y2)]]))

(defn- read-input []
  (->> "inputs/day5"
       slurp
       str/split-lines
       (map parse-coordinates)))

(defn- keep-non-diagonal [coordinates]
  (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))) coordinates))

(defn- build-line [[[x1 y1] [x2 y2]]]
  (let [[x1 x2] (sort [x1 x2])
        [y1 y2] (sort [y1 y2])]
    (for [x (range x1 (inc x2))
          y (range y1 (inc y2))]
      [x y])))

(defn- mark-line [field line]
  (let [fr (frequencies line)]
    (merge-with (fnil + 1) field fr)))

(defn- mark-lines [field lines]
  (reduce mark-line field lines))

(defn- create-field [input]
  (->> input
       keep-non-diagonal
       (map build-line)
       (mark-lines {})))

(defn- render [size points]
  (let [field (vec (repeat 10 (vec (repeat 10 "."))))]
    (->> points
         (reduce (fn [field [[x y] val]]
                   (assoc-in field [x y] (str val)))
                 field)
         (apply map vector)
         (map str/join)
         (str/join "\n")
         println)))

(defn- part-1 [input]
  (->> (create-field input)
       vals
       (filter #(> % 1))
       count))

(defn- build-diagonale-line [[[x1 y1] [x2 y2]]]
  (when (= (Math/abs (- x1 x2)) (Math/abs (- y1 y2)))
    (let [xdir (if (> 0 (- x2 x1)) -1 1)
          ydir (if (> 0 (- y2 y1)) -1 1)]
      (loop [line [[x1 y1]]]
        (let [[x y] (last line)]
          (if (and (not= x x2) (not= y y2))
            (recur (conj line [(+ x xdir) (+ y ydir)]))
            line))))))

(defn- part-2 [input]
  (->> input
       (keep build-diagonale-line)
       (mark-lines (create-field input))
       vals
       (filter #(> % 1))
       count))

(defn hydrothermal-venture []
  (println "Hydrothermal Venture")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 5280
    (println "  part two:" (part-2 input)))) ; 16716


(ns aoc2021
  (:require day1 day2 day3 day4 day5))

(day1/sonar-sweep)
(day2/dive!)
(day3/binary-diagnostic)
(day4/giant-squid)
(day5/hydrothermal-venture)
