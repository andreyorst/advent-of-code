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

(defn run [& _]
  (println "Day 4 - Passport Processing")
  (let [passport-specs (read-passport-data "inputs/day4.txt")]
    (println "  part one:" (count-passports-w-all-fields passport-specs)) ; 196
    (println "  part two:" (count-valid-passports passport-specs)))) ; 114
