(ns day12
  (:require [clojure.string :as str]))

(defn- read-input []
  (->> "inputs/day12"
       slurp
       str/split-lines
       (map #(str/split % #"-"))
       (reduce
        (fn [map [start end]]
          (-> map
              (update start (fnil conj #{}) end)
              (update end (fnil conj #{}) start)))
        {})))

(defn- lower-case? [s]
  (= s (str/lower-case s)))

(defn- all-possible-steps [routes path]
  (let [seen (frequencies (filter lower-case? path))]
    (filter some? (for [end (routes (last path))]
                    (when (not (seen end))
                      (str/join "," (conj path end)))))))

(defn- build-paths [routes steps]
  (loop [paths [["start"]]
         done []]
    (let [paths (->> paths
                     (map #(steps routes %))
                     flatten
                     (filter some?)
                     (map #(str/split % #",")))]
      (if (seq paths)
        (recur (remove #(= (last %) "end") paths)
               (concat done (filter #(= (last %) "end") paths)))
        done))))

(defn- all-possible-steps-2 [routes path]
  (let [seen (frequencies (filter lower-case? path))
        vn (reduce + (vals seen))
        kn (count (keys seen))]
    (when (or (= vn kn)
              (= vn (inc kn)))
      (->> path
           last
           routes
           (map #(when (not= % "start")
                   (str/join "," (conj path %))))))))

(defn- part-1 [input]
  (count (build-paths input all-possible-steps)))

(defn- part-2 [input]
  (count (build-paths input all-possible-steps-2)))

(defn run [& _]
  (println "Day 12 - Passage Pathing")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))            ; 5178
    (println "  part two:" (part-2 input))))          ; 130094
