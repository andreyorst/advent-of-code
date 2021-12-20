(ns day20
  (:require [clojure.string :as str]))

(defn- img->coords [img]
  (reduce-kv (fn [img y line]
               (reduce-kv (fn [img x c]
                            (assoc img [x y] (if (= c \#) 1 0)))
                          img (vec line)))
             (sorted-map)
             (str/split img #"\n")))

(defn- read-input []
  (let [[enh img] (-> "inputs/day20"
                      slurp
                      (str/split #"\n\n"))]
    {:enhance (mapv #(if (= % \#) 1 0) enh)
     :image (img->coords img)}))

(defn- enhancement [{:keys [image enhancement]} [x y] default]
  (nth (-> (for [y [(dec y) y (inc y)]
                 x [(dec x) x (inc x)]]
             (get image [x y] default))
           str/join
           (Long/parseLong 2))
       enhancement))

(defn- enhance [{:keys [enhance image] :as img} step]
  (let [points (keys image)
        [min-x min-y] (first points)
        [max-x max-y] (last points)
        default (cond (= step 0) 0
                      (even? step) (peek enhance)
                      :else (first enhance))]
    {:image (into
             (sorted-map)
             (for [x (range (dec min-x) (+ max-x 2))
                   y (range (dec min-y) (+ max-y 2))]
               [[x y] (enhancement img [x y] default)]))
     :enhance enhance}))

(defn- part-1 [input]
  (->> (reduce #(enhance %1 %2) input (range 2))
       :image
       vals
       (reduce +)))

(defn- part-2 [input]
  (->> (reduce #(enhance %1 %2) input (range 50))
       :image
       vals
       (reduce +)))

(defn run [& _]
  (println "Day 20 - Trench Map")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 5619
    (println "  part two:" (part-2 input)))) ; 20122
