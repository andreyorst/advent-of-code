(ns day8
  (:require [aoc-commons :refer [parse-long slurp-lines]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn- read-input []
  (->> "inputs/day8"
       slurp-lines
       (map (fn [line]
              (let [[signals digits] (str/split line #"\s+\|\s+")]
                {:signals (str/split signals #"\s+")
                 :digits (str/split digits #"\s+")})))))

(defn- part-1 [input]
  (->> input
       (map :digits)
       (mapcat #(map count %))
       (keep #{2 3 4 7})
       count))

(defn- map-signals-to-digits [signals]
  (let [signals (map set signals)
        one (some #({2 %} (count %)) signals)
        four (some #({4 %} (count %)) signals)
        seven (some #({3 %} (count %)) signals)
        eight (some #({7 %} (count %)) signals)
        a (first (set/difference seven one))
        g (some #(when-let [c (and (= 6 (count %))
                                   (set/difference % (set/union seven four)))]
                   (when (= 1 (count c))
                     (first c)))
                signals)
        nine (set/union #{g} seven four)
        e (first (set/difference eight nine))
        six (some #(and (= (count %) 6)
                        (seq (set/intersection one (set/difference eight (set %))))
                        %)
                  signals)
        c (first (set/difference eight six))
        f (first (disj one c))
        five (disj six e)
        two (some #(and (= (count %) 5)
                        (= #{c e} (set/difference % five))
                        %)
                  signals)
        three (conj (disj two e) f)
        d (first (disj two a c e g))
        zero (disj eight d)]
    (zipmap [zero one two three four
             five six seven eight nine]
            (range 10))))

(defn- decode [row]
  (let [{:keys [signals digits]} row
        signals->digits (map-signals-to-digits signals)]
    (->> digits
         (map set)
         (map signals->digits)
         (str/join)
         parse-long)))

(defn- part-2 [input]
  (->> input
       (map decode)
       (reduce + )))

(defn run [& _]
  (println "Day 8 - Seven Segment Search")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 381
    (println "  part two:" (part-2 input)))) ; 1023686
