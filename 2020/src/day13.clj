(ns day13
  (:require [clojure.string :as str]))

(defn- read-schedule [input]
  (let [[estimate ids] (-> input
                           slurp
                           (str/split-lines))]
    {:estimate (Integer. estimate)
     :ids (->> (str/split ids #",")
               (map #(if (re-find #"\d+" %) (Integer. %) (keyword %))))}))

(defn- earliest-bus [{:keys [estimate ids]}]
  (let [[id times] (->> ids
                        (filter pos-int?)
                        (map #(do [% (int (/ estimate %))]))
                        (sort (fn [[_ a] [_ b]] (< a b)))
                        first)
        timestamp (if (< (* id times) estimate)
                    (* id (inc times))
                    (* id times))]
    (* (- timestamp estimate) id)))

(defn- continious-bus-departures [{:keys [ids]}]
  (let [time-slots (->> ids
                        (map-indexed vector)
                        (filter (fn [[_ id]] (pos-int? id))))
        #_#_ id (second (first time-slots))]
    ;; Works for examples, but too slow for the real input
    #_(loop [timestamp id]
        (if (every? (fn [[delta id]]
                      (zero? (mod (+ timestamp delta) id)))
                    time-slots)
          timestamp
          (recur (+ timestamp id))))
    (->> time-slots
         (reduce (fn [[timestamp next-time] [delta id]]
                   (if (zero? (mod (+ timestamp delta) id))
                     [timestamp (* next-time id)]
                     (recur [(+ timestamp next-time) next-time]
                            [delta id]))))
         first)))

(defn run [& _]
  (println "Day 13 - Shuttle Search")
  (let [schedule (read-schedule "inputs/day13.txt")]
    (println "  part one:" (earliest-bus schedule)) ; 6559
    (println "  part two:" (continious-bus-departures schedule)))) ; 626670513163231
