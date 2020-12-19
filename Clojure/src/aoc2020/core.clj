(ns day1
  (:require [clojure.string :as str]))

(defn- fix-report-two-nums
  ([report]
   (fix-report-two-nums report 0))
  ([report x]
   (when-let [f (first report)]
     (let [report (disj report f)]
       (if-let [s (report (- 2020 f x))]
         (* f s)
         (recur report x))))))

(defn- fix-report-three-nums [report]
  (when-let [f (first report)]
    (let [report (disj report f)]
      (if-let [res (fix-report-two-nums report f)]
        (* res f)
        (recur report)))))

(defn report-repair []
  (println "Day 1 - Report Repair")
  (let [report (->> "../inputs/day1.txt"
                    slurp
                    str/split-lines
                    (map #(Integer. %))
                    (into #{}))]
    (println "  part one:" (fix-report-two-nums report))
    (println "  part two:" (fix-report-three-nums report))))


(ns day2
  (:require [clojure.string :as str]))

(defn- read-password-spec [passwords]
  (->> passwords
       slurp
       str/split-lines
       (map #(->> %
                  (re-find #"(\d+)-(\d+)\s+(\w):\s*(.*)")
                  ((fn [[_ lower upper letter passwd]]
                     {:lower (Integer. lower)
                      :upper (Integer. upper)
                      :letter letter
                      :passwd passwd}))))))

(defn- count-old-valid-passwords [password-spec]
  (->> password-spec
       (filter (fn [{:keys [lower upper letter passwd]}]
                 (let [appeared (count (re-seq (re-pattern letter) passwd))]
                   (<= lower appeared upper))))
       count))

(defn- count-new-valid-passwords [password-spec]
  (->> password-spec
       (filter (fn [{:keys [lower upper letter passwd]}]
                 (let [passwd (into [] passwd)]
                   (= (hash-set (= (str (passwd (dec lower))) letter)
                                (= (str (passwd (dec upper))) letter))
                      #{true false}))))
       count))

(defn password-philosophy []
  (println "Day 2 - Password Philosophy")
  (let [password-spec (read-password-spec "../inputs/day2.txt")]
    (println "  part one:" (count-old-valid-passwords password-spec))
    (println "  part two:" (count-new-valid-passwords password-spec))))


(ns day3
  (:require [clojure.string :as str]))

(defn- read-tree-map [map]
  (let [lines (->> map
                   slurp
                   str/split-lines)]
    {:width (-> lines first count)
     :rows (mapv #(into [] %) lines)}))

(defn- traverse [right down map]
  (loop [rows (drop down (:rows map))
         pos 0
         trees 0]
    (if-let [line (first rows)]
      (let [new-pos (mod (+ pos right) (:width map))]
        (recur (drop down rows)
               new-pos
               (if (= (line new-pos) \#) (inc trees) trees)))
      trees)))

(defn toboggan-trajectory []
  (println "Day 3 - Toboggan Trajectory")
  (let [map-data (read-tree-map "../inputs/day3.txt")]
    (println "  part one:" (traverse 3 1 map-data))
    (println "  part two:" (* (traverse 1 1 map-data)
                                  (traverse 3 1 map-data)
                                  (traverse 5 1 map-data)
                                  (traverse 7 1 map-data)
                                  (traverse 1 2 map-data)))))


(ns day4
  (:require [clojure.string :as str]))

(defn- read-passport-data [input]
  (map (fn [line]
         (->> line
              (re-seq #"([a-z]{3}):([^\s]+)")
              (mapcat #(->> % (drop 1) (partition 2)))
              (map vec)
              (into {})))
       (-> input slurp (str/split #"\n\n"))))

(defn- ->year [s]
  (and (re-matches #"\d{4}" s) (Integer. s)))

(defmulti ^:private valid-entry? (fn [[k _]] k))

(defmethod valid-entry? "byr" [[_ v]]
  (when-let [year (->year v)]
    (<= 1920 year 2002)))

(defmethod valid-entry? "iyr" [[_ v]]
  (when-let [year (->year v)]
    (<= 2010 year 2020)))

(defmethod valid-entry? "eyr" [[_ v]]
  (when-let [year (->year v)]
    (<= 2020 year 2030)))

(defmethod valid-entry? "hgt" [[_ v]]
  (when-let [[_ num measure] (re-find #"([0-9]+)(cm|in)" v)]
    (condp = measure
      "cm" (<= 150 (Integer. num) 193)
      "in" (<= 59 (Integer. num) 76)
      nil)))

(defmethod valid-entry? "hcl" [[_ v]]
  (re-matches #"#[0-9a-f]{6}" v))

(defmethod valid-entry? "ecl" [[_ v]]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v))

(defmethod valid-entry? "pid" [[_ v]]
  (re-matches #"[0-9]{9}" v))

(defmethod valid-entry? "cid" [[_ _]] true)

(defmethod valid-entry? :default [[_ _]] nil)

(defn- has-all-entries?
  [passport]
  (= (into #{"cid"} (keys passport))
     #{"byr" "iyr" "eyr" "hgt"
       "hcl" "ecl" "pid" "cid"}))

(defn- count-passports-w-all-fields [passport-specs]
  (->> passport-specs
       (filter has-all-entries?)
       count))

(defn- count-valid-passports [passport-specs]
  (->> passport-specs
       (filter #(and (has-all-entries? %)
                     (every? valid-entry? %)))
       count))

(defn passport-processing []
  (println "Day 4 - Passport Processing")
  (let [passport-specs (read-passport-data "../inputs/day4.txt")]
    (println "  part one:" (count-passports-w-all-fields passport-specs))
    (println "  part two:" (count-valid-passports passport-specs))))


(ns day5
  (:require [clojure.string :as str]))

(defn- read-boarding-list [boarding]
  (->> boarding
       slurp
       str/split-lines))

(defn- average [col]
  (int (float (/ (apply + col) (count col)))))

(defn- calc-id [scheme]
  (loop [scheme scheme
         row [1 128]
         col [1 8]]
    (if (seq scheme)
      (recur (rest scheme)
             (condp = (first scheme)
               \F [(row 0) (average row)]
               \B [(average row) (row 1)]
               row)
             (condp = (first scheme)
               \L [(col 0) (average col)]
               \R [(average col) (col 1)]
               col))
      (+ (* (dec (row 1))
            8)
         (dec (col 1))))))

(defn- my-seat [ids]
  (let [seats (sort ids)
        low (apply min seats)]
    (reduce (fn [current next]
              (if (not= (inc current) next)
                (reduced (inc current))
                next))
            low
            (drop 1 seats))))

(defn binary-boarding []
  (println "Day 5 - Binary Boarding")
  (let [boardings (read-boarding-list "../inputs/day5.txt")
        seat-ids (map calc-id boardings)]
    (println "  part one:" (apply max seat-ids))
    (println "  part two: " (my-seat seat-ids))))


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

(defn custom-customs []
  (println "Day 6 - Custom Customs")
  (let [answers (read-answers "../inputs/day6.txt")]
    (println "  part one:" (count-group-answers answers))
    (println "  part two:" (count-simultaneous-answers answers))))


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

(defn handy-haversacks []
  (println "Day 7 - Handy Haversacks")
  (let [bags (read-bag-info "../inputs/day7.txt")]
    (println "  part one:" (count-bags bags "shiny gold"))))


(ns aoc2020.core
  (:require [day1] [day2] [day3] [day4] [day5] [day6] [day7])
  (:gen-class))

(defn -main [& _]
  (day1/report-repair)
  (day2/password-philosophy)
  (day3/toboggan-trajectory)
  (day4/passport-processing)
  (day5/binary-boarding)
  (day6/custom-customs)
  (day7/handy-haversacks))
