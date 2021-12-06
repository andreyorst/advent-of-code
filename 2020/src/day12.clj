(ns day12
  (:require [clojure.string :as str]))

(defn- read-directions
  "Transform 7-directional coordinates into 5-directional coordinates.
  Positive West coordinates become negative East coordinates.
  Positive South coordinates become negative North coordinates.
  Positive Left rotations become negative Right rotations.

      N 1
      |
  W --+-- E
  -1  |   1
      S -1"
  [input]
  (->> input
       slurp
       str/split-lines
       (map #(let [[_ dir amount] (re-find #"([LRFSEWN])(\d+)" %)]
               [(keyword (str/lower-case (cond (= dir "W") "E"
                                               (= dir "S") "N"
                                               (= dir "L") "R"
                                               :else dir)))
                ((if (or (= dir "S") (= dir "W") (= dir "L")) - +) (Integer. amount))]))))

(defn- move-ship [{:keys [n e angle] :as ship} [direction & directions]]
  (if-let [[direction amount] direction]
    (recur (case direction
             (:n :e) (update ship direction #(+ amount %))
             :r (update ship :angle #(mod (+ % amount) 360))
             :f (condp = angle
                  0   (update ship :n #(+ % amount))
                  90  (update ship :e #(+ % amount))
                  180 (update ship :n #(- % amount))
                  270 (update ship :e #(- % amount))))
           directions)
    (+ (Math/abs n) (Math/abs e))))

(defn- move-ship-to-waypoint [{:keys [n e wn we] :as ship} [direction & directions]]
    (if-let [[direction amount] direction]
      (recur (case direction
               :n (assoc ship :wn (+ amount wn))
               :e (assoc ship :we (+ amount we))
               (:r :l) (condp = (mod amount 360)
                         0   ship
                         90  (assoc ship :wn (- we) :we wn)
                         180 (assoc ship :wn (- wn) :we (- we))
                         270 (assoc ship :wn we :we (- wn)))
               :f (assoc ship :n (+ (* wn amount) n) :e (+ (* we amount) e)))
             directions)
      (+ (Math/abs n) (Math/abs e))))

(defn run [& _]
  (println "Day 12 - Rain Risk")
  (let [directions (read-directions "inputs/day12.txt")]
    (println "  part one:" (move-ship {:n 0 :e 0 :angle 90} directions)) ; 879
    (println "  part two:" (move-ship-to-waypoint {:n 0 :e 0 :wn 1 :we 10} directions)))) ; 18107
