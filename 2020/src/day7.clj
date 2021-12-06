(ns day7
  (:require [clojure.string :as str]))

(defn- parse-bag-spec [spec]
  (let [main-color-re #"(\w+\s+\w+)\s+bags\s+contain"
        inner-color-re #"(?:\s+\d+\s+\w+\s+\w+\s+bags?[.,])+"
        no-bags-re #"(?:\s+no\s+other\s+bags)"
        bag-re (re-pattern (format "^%s(%s|%s)" main-color-re inner-color-re no-bags-re))
        [_ color amounts] (re-find bag-re spec)]
    [color (if (re-find no-bags-re amounts)
             {}
             (->> (str/split amounts #"[.,]\s+")
                  (map #(let [[_ amount color] (re-find #"\s*(\d+)\s+(\w+\s+\w+)" %)]
                          [color (Integer. amount)]))
                  (into {})))]))

(defn- read-bag-info [input]
  (->> input
       slurp
       str/split-lines
       (map parse-bag-spec)
       (into {})))

(defn- check-bag [[bag-color inner-bags] color bags]
  (if (inner-bags color)
    bag-color
    (map #(check-bag [bag-color (bags (first %))] color bags) inner-bags)))

(defn- count-bags [bags color]
  (count (into #{} (flatten (map #(check-bag % color bags) bags)))))

(defn- count-inner [bags color]
  (let [inner-bags (bags color)]
    (if (empty? inner-bags) 1
        (reduce + 1 (map (fn [[color amount]]
                           (* amount (count-inner bags color))) inner-bags)))))

(defn run [& _]
  (println "Day 7 - Handy Haversacks")
  (let [bags (read-bag-info "inputs/day7.txt")]
    (println "  part one:" (count-bags bags "shiny gold")) ; 128
    (println "  part two:" (dec (count-inner bags "shiny gold"))))) ; 20189
