(ns day21
  (:require [clojure.string :as str]
            [aoc-commons :refer [parse-long]]))

(defn read-input []
  (reduce #(let [[_ player pos] (re-find #".*(\d+).*(\d+)" %2)
                 player (parse-long player)
                 pos (parse-long pos)]
             (assoc %1 player
                    {:pos pos
                     :score 0}))
          (sorted-map)
          (->> "inputs/day21"
               slurp
               str/split-lines)))

(defn step [[throws players turns] [i player]]
  (let [pos (inc (mod (+ (:pos player) (dec (first turns))) 10))
        score (+ (:score player) pos)]
    ((if (>= score 1000)
       reduced
       identity)
     [(+ throws 3)
      (update players i
              assoc
              :pos pos
              :score score)
      (rest turns)])))

(defn part-1 [input]
  (loop [throws 0
         rolls (map (partial apply +) (partition 3 (cycle (range 1 101))))
         players input]
    (let [[throws players turns] (reduce step [throws players rolls] players)]
      (if (some #(>= (:score (second %)) 1000) players)
        (* throws (apply min (map (comp :score second) players)))
        (recur throws
               turns
               players)))))

(def play
  (memoize
   (fn [players turn]
     (loop [[roll & rolls] (for [a [1 2 3] b [1 2 3] c [1 2 3]] [a b c])
            wins {}]
       (if roll
         (let [roll (apply + roll)
               p (players turn)
               pos (inc (mod (+ (:pos p) (dec roll)) 10))
               score (+ (:score p) pos)]
           (if (> score 20)
             (recur rolls (update wins turn (fnil inc 0)))
             (let [players (assoc players turn {:pos pos :score score})
                   wins' (play players
                               (inc (mod turn (count players))))]
               (recur rolls (merge-with + wins wins')))))
         wins)))))

(defn part-2 [input]
  (apply max (vals (play input 1))))

(defn run [& _]
  (println "Day 21 - Dirac Dice")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 995904
    (println "  part two:" (part-2 input)))) ; 193753136998081
