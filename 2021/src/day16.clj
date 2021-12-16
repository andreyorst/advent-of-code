(ns day16
  (:require [clojure.string :as str]))

(def hex-to-binary
  {\0 "0000", \1 "0001", \2 "0010", \3 "0011",
   \4 "0100", \5 "0101", \6 "0110", \7 "0111",
   \8 "1000", \9 "1001", \A "1010", \B "1011",
   \C "1100", \D "1101", \E "1110", \F "1111"})

(defn read-input []
  (->> "inputs/day16"
       slurp
       (mapcat hex-to-binary)))

(defn parse-binary [b]
  (Long/parseLong (str/join b) 2))

(defn read-version-or-id [packet]
  (let [[ver-or-id packet] (split-at 3 packet)]
    [(parse-binary ver-or-id) packet]))

(defn read-value [packet]
  (loop [[continue? a b c d & packet] packet
         v ""]
    (if (= \1 continue?)
      (recur packet
             (str v a b c d))
      [(parse-binary (str v a b c d)) packet])))

(defn read-length [packet]
  (let [[type & packet] packet
        id (parse-binary (str type))
        length (case id 0 15 1 11)
        [length packet] (split-at length packet)]
    [id (parse-binary length) packet]))

(defn process-packet-stream [packets]
  (let [[ver packets] (read-version-or-id packets)
        [id packets] (read-version-or-id packets)]
    (if (= id 4)
      (let [[val packets] (read-value packets)]
        [[ver] [val] packets])
      (let [[length-id length packets] (read-length packets)
            [vers vals packets]
            (loop [length length
                   packets packets
                   inner-vers [ver]
                   inner-vals []]
              (if (> length 0)
                (let [len-before (count packets)
                      [vers vals packets] (process-packet-stream packets)]
                  (recur (case length-id
                           0 (- length (- len-before (count packets)))
                           1 (dec length))
                         packets
                         (into inner-vers vers)
                         (into inner-vals vals)))
                [inner-vers inner-vals packets]))]
        (case id
          0 [vers [(reduce + vals)] packets]
          1 [vers [(reduce * vals)] packets]
          2 [vers [(apply min vals)] packets]
          3 [vers [(apply max vals)] packets]
          5 [vers [(if (apply > vals) 1 0)] packets]
          6 [vers [(if (apply < vals) 1 0)] packets]
          7 [vers [(if (apply = vals) 1 0)] packets])))))

(defn part-1 [input]
  (->> input
       process-packet-stream
       first
       (reduce +)))

(defn part-2 [input]
  (->> input
       process-packet-stream
       second
       first))

(defn run [& _]
  (println "Day 16 - Packet Decoder")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 879
    (println "  part two:" (part-2 input)))) ; 539051801941
