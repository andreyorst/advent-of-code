(ns day6
  (:require [clojure.string :as str]))

(defn- read-answers [answers]
  (-> answers
      slurp
      (str/split #"\n\n")))

(defn- count-group-answers [answers]
  (->> answers
       (map (fn [group]
              (->> group
                   str/split-lines
                   str/join
                   (into #{})
                   count)))
       (apply +)))

(defn- count-simultaneous-answers [answers]
  (->> answers
       (map (fn [group]
              (let [answers (->> (str/split-lines group)
                                 (map #(into #{} %)))]
                (if (= (count answers) 1)
                  (count (first answers))
                  (loop [answered (first answers)
                         answers (rest answers)]
                    (if (seq answers)
                      (recur (->> (map answered (first answers))
                                  (filter (complement nil?))
                                  (into #{}))
                             (rest answers))
                      (count answered)))))))
       (apply +)))

(defn run [& _]
  (println "Day 6 - Custom Customs")
  (let [answers (read-answers "inputs/day6.txt")]
    (println "  part one:" (count-group-answers answers)) ; 7027
    (println "  part two:" (count-simultaneous-answers answers)))) ; 3579
