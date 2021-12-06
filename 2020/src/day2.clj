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

(defn run [& _]
  (println "Day 2 - Password Philosophy")
  (let [password-spec (read-password-spec "inputs/day2.txt")]
    (println "  part one:" (count-old-valid-passwords password-spec)) ; 434
    (println "  part two:" (count-new-valid-passwords password-spec)))) ; 509
