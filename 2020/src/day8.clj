(ns day8
  (:require [clojure.string :as str]))

(defn- read-opcodes [input]
  (->> input
       slurp
       str/split-lines
       (map #(let [[_ op num] (re-find #"(\w+)\s+([-+]\d+)" %)]
               [op (Integer. num)]))
       (into [])))

(defn- detect-loop [programm]
  (let [size (count programm)]
    (loop [acc 0
           line 0
           visited #{}]
      (cond (visited line) [:loop acc]
            (>= line size) [:end acc]
            :else (let [line (if (< line 0) 0 line)
                        [op num] (programm line)]
                    (condp = op
                      "acc" (recur (+ acc num) (inc line) (conj visited line))
                      "nop" (recur acc (inc line) (conj visited line))
                      "jmp" (recur acc (+ line num) (conj visited line))))))))

(defn- fix-boot [programm]
  (let [size (count programm)]
    (loop [line 0]
      (let [[op num] (programm line)]
        (cond (and (= op "nop") (not= num 0))
              (let [[case acc] (detect-loop (assoc-in programm [line 0] "jmp"))]
                (if (= case :loop) (recur (inc line))
                    acc))
              (= op "jmp")
              (let [[case acc] (detect-loop (assoc-in programm [line 0] "nop"))]
                (if (= case :loop) (recur (inc line))
                    acc))
              (< line size) (recur (inc line)))))))

(defn run [& _]
  (println "Day 8 - Handheld Halting")
  (let [programm (read-opcodes "inputs/day8.txt")]
    (println "  part one:" (second (detect-loop programm))) ; 1709
    (println "  part two:" (fix-boot programm))))           ; 1976
