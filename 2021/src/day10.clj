(ns day10
  (:require [clojure.string :as str]))

(defn- read-input []
  (->> "inputs/day10"
       slurp
       str/trim
       str/split-lines))

(def ^:private matching
  {\( \), \{ \}, \[ \], \< \>})

(def ^:private incorrect-score
  {\) 3, \] 57, \} 1197, \> 25137})

(def ^:private correct-score
  {\) 1, \] 2, \} 3, \> 4})

(defn- to-stack-or-error-char [line]
  (loop [stack ()
         [paren & parens] line]
    (case paren
      (\( \[ \{ \<) (recur (cons paren stack) parens)
      (\) \] \} \>) (if (= (matching (first stack)) paren)
                      (recur (rest stack) parens)
                      paren)
      stack)))

(defn- part-1 [input]
  (->> input
       (keep to-stack-or-error-char)
       (filter char?)
       (map incorrect-score)
       (reduce +)))

(defn- get-middle [list]
  (first (drop (quot (count list) 2) list)))

(defn- score-completion-stack [stack]
  (->> stack
       (map matching)
       (map correct-score)
       (reduce (fn [total score]
                 (+ (* total 5) score)))))

(defn- part-2 [input]
  (->> input
       (map to-stack-or-error-char)
       (filter coll?)
       (map score-completion-stack)
       sort
       get-middle))

(defn run [& _]
  (println "Day 10 - Syntax Scoring")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 318099
    (println "  part two:" (part-2 input)))) ; 2389738699
