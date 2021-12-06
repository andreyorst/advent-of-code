(ns day11
  (:require [clojure.string :as str]))

(defn- read-seat-data [input]
  (->> input
       slurp
       str/split-lines
       (map #(into [] (str/split % #"")))
       (into [])))

(defn- occupied? [seat]
  (if (and seat (= seat "#")) 1 0))

(defn- seat-liberties [x y seats]
  (+ (occupied? (get-in seats [(inc x) y]))
     (occupied? (get-in seats [(dec x) y]))
     (occupied? (get-in seats [x (inc y)]))
     (occupied? (get-in seats [x (dec y)]))
     (occupied? (get-in seats [(inc x) (inc y)]))
     (occupied? (get-in seats [(dec x) (dec y)]))
     (occupied? (get-in seats [(dec x) (inc y)]))
     (occupied? (get-in seats [(inc x) (dec y)]))))

(defn- can-occupy? [seat liberty]
  (and (= seat "L") (= liberty 0)))

(defn- should-leave? [seat liberty max-liberty]
  (and (= seat "#") (>= liberty max-liberty)))

(defn- occupy-seats [seats]
  (let [new-state (atom seats)]
    (dotimes [x (count seats)]
      (dotimes [y (count (first seats))]
        (let [seat (get-in seats [x y])
              liberty (seat-liberties x y seats)]
          (swap! new-state assoc-in [x y]
                 (cond
                   (should-leave? seat liberty 4) "L"
                   (can-occupy? seat liberty) "#"
                   :else seat)))))
    @new-state))


(defn- wide-occupied? [x y dir-x dir-y seats]
  (loop [x (+ dir-x x)
         y (+ dir-y y)]
    (if-let [seat (get-in seats [x y])]
      (cond
        (= "#" seat) 1
        (= "L" seat) 0
        :else (recur (+ x dir-x) (+ y dir-y)))
      0)))

(defn- seat-wide-liberties [x y seats]
  (+ (wide-occupied? x y  1  0 seats)
     (wide-occupied? x y -1  0 seats)
     (wide-occupied? x y  0  1 seats)
     (wide-occupied? x y  0 -1 seats)
     (wide-occupied? x y  1  1 seats)
     (wide-occupied? x y -1 -1 seats)
     (wide-occupied? x y -1  1 seats)
     (wide-occupied? x y  1 -1 seats)))

(defn- wide-occupy-seats [seats]
  (let [new-state (atom seats)]
    (dotimes [x (count seats)]
      (dotimes [y (count (first seats))]
        (let [seat (get-in seats [x y])
              liberty (seat-wide-liberties x y seats)]
          (swap! new-state assoc-in [x y]
                 (cond
                   (should-leave? seat liberty 5) "L"
                   (can-occupy? seat liberty) "#"
                   :else seat)))))
    @new-state))

(defn- simulate-crowd [seats occupy-fn]
  (let [new-state (atom (occupy-seats seats))
        seats (atom seats)]
    (while (not= @new-state @seats)
      (reset! seats @new-state)
      (reset! new-state (occupy-fn @new-state)))
    (count (mapcat (fn [row] (filter #(= "#" %) row)) @new-state))))

(defn run [& _]
  (println "Day 11 - Seating System")
  (let [seats (read-seat-data "inputs/day11.txt")]
    (println "  part one:" (simulate-crowd seats occupy-seats)) ; 2324
    (println "  part two:" (simulate-crowd seats wide-occupy-seats)))) ; 2068
