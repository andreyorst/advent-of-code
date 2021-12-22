(ns day22
  (:require [clojure.string :as str]
            [aoc-commons :refer [parse-long]]))

(defn read-input []
  (->> "inputs/day22"
       slurp
       str/split-lines
       (reduce
        (fn [data line]
          (let [[_ cmd & coords]
                (re-find #"(on|off).*x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)" line)
                [lx ux ly uy lz uz] (map parse-long coords)]
            (-> data
                (assoc
                 :min-x (min (:min-x data lx) lx)
                 :max-x (max (:max-x data ux) ux)
                 :min-y (min (:min-y data ly) ly)
                 :max-y (max (:max-y data uy) uy)
                 :min-z (min (:min-z data lz) lz)
                 :max-z (max (:max-z data uz) uz))
                (update :commands conj
                        (fn [cubes [x y z]]
                          (if (and (<= lx x ux) (<= ly y uy) (<= lz z uz))
                            (assoc cubes [x y z] cmd)
                            cubes))))))
        {:commands []})))

(defn make-cubes [{:keys [min-x max-x min-y max-y min-z max-z]}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))
        z (range min-z (inc max-z))]
    [x y z]))

(defn reboot [cubes commands]
  (reduce (fn [state command]
            (reduce command state cubes))
          {} commands))

(defn part-1 [input]
  (->> input
       :commands
       (reboot (make-cubes {:min-x -50 :max-x 50
                            :min-y -50 :max-y 50
                            :min-z -50 :max-z 50}))
       (filter #(= (second %) "on"))
       count))

(defn run [& _]
  (println "Day 22 - Reactor Reboot")
  (let [input (read-input)]
    (println "  part one:" (part-1 input)) ; 587785
    #_(println "  part two:" (part-1 input))))
