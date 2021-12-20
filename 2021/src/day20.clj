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

(defn- enhancement-pos [{:keys [image enhance]} [x y] step]
  (let [default (cond (= step 0) 0
                      (even? step) (peek enhance)
                      :else (first enhance))]
    (-> (for [ydir [-1 0 1]
              xdir [-1 0 1]]
          (get image [(+ x xdir) (+ y ydir)] default))
        str/join
        (Long/parseLong 2))))

(defn- enhance [{:keys [enhance image] :as img} step]
  (let [points (keys image)
        [min-x min-y] (first points)
        [max-x max-y] (last points)]
    {:image (into
             (sorted-map)
             (for [x (range (dec min-x) (+ max-x 2))
                   y (range (dec min-y) (+ max-y 2))]
               [[x y] (nth enhance (enhancement-pos img [x y] step))]))
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

#_
(defn render [image]
  (let [points (sort (keys image))
        [min-x min-y] (first points)
        [max-x max-y] (last points)
        adj-x (if (< min-x 0) (- min-x) 0)
        adj-y (if (< min-y 0) (- min-y) 0)
        field (into [] (repeat (inc (+ max-y adj-y)) (into [] (repeat (inc (+ max-x adj-x)) "."))))]
    (->> (keys image)
         (reduce (fn [field [x y]]
                   (assoc-in field [(+ adj-y y) (+ adj-x x)]
                             (if (= (image [x y]) 1) "#" ".")))
                 field)
         (map str/join)
         (str/join "\n")
         println)
    (println)))
