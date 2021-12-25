(ns day18
  (:require [clojure.zip :as z]
            [clojure.string :as str]))

(defn- read-input []
  (-> "inputs/day18"
       slurp
       (str/join "[]")
       read-string))

(defn- exploding-node [root]
  (loop [level 0
         loc (z/vector-zip root)]
    (cond (and (= level 4) (z/branch? loc))
          loc
          (z/branch? loc)
          (recur (inc level) (z/down loc))
          (z/right loc) (recur level (z/right loc))
          :else
          (let [[level loc]
                (loop [level (dec level)
                       loc (z/up loc)]
                  (cond (and (= level 0) (not (z/right loc)))
                        [level nil]
                        (not (z/right loc))
                        (recur (dec level) (z/up loc))
                        :else
                        [level (z/right loc)]))]
            (when loc
              (recur level loc))))))

(defn- find-right [loc]
  (when-not (= (z/root loc) (z/node loc))
    (if-let [right (z/right loc)]
      (if (z/branch? right)
        (loop [loc right]
          (if (z/branch? loc)
            (recur (z/next loc))
            loc))
        right)
      (recur (z/up loc)))))

(defn- find-left [loc]
  (when-not (= (z/root loc) (z/node loc))
    (if-let [left (z/left loc)]
      (if (z/branch? left)
        (loop [loc (z/next left)]
          (let [loc (z/rightmost loc)]
            (if (z/branch? loc)
              (recur (z/next loc))
              loc)))
        left)
      (recur (z/up loc)))))

(defn- explode [root]
  (if-let [loc (exploding-node root)]
    (let [[a b] (z/node loc)
          left (find-left loc)
          right (find-right loc)]
      (-> (cond (not left) (z/edit right + b)
                (not right) (z/edit left + a)
                :else (-> left
                          (z/edit + a)
                          z/root
                          exploding-node
                          find-right
                          (z/edit + b)))
          z/root
          exploding-node
          (z/replace 0)
          z/root))
    root))

(defn- splittable-node [root]
  (loop [loc (z/vector-zip root)]
    (when-not (z/end? loc)
      (cond (z/branch? loc)
            (recur (z/next loc))
            (> (z/node loc) 9)
            loc
            :else
            (recur (z/next loc))))))

(defn- split [root]
  (if-let [loc (splittable-node root)]
    (-> loc
        (z/edit (juxt #(int (Math/floor (/ % 2)))
                      #(int (Math/ceil (/ % 2)))))
        z/root)
    root))

(defn- sum [a b]
  (loop [x [a b]]
    (let [x' (explode x)]
      (if (= x' x)
        (let [x' (split x')]
          (if (= x x')
            x'
            (recur x')))
        (recur x')))))

(defn- magnitude' [loc]
  (cond (z/end? loc)
        (z/root loc)
        (z/branch? loc)
        (let [loc (z/next loc)
              left (magnitude' loc)
              right (magnitude' (z/right loc))]
          (z/replace loc (+ (* 3 (z/node left)) (* 2 (z/node right)))))
        :else
        loc))

(defn- magnitude [root]
  (z/node (magnitude' (z/vector-zip root))))

(defn- part-1 [input]
  (->> input
       (reduce sum)
       magnitude))

(defn- part-2 [input]
  (->> (for [a input
             b input]
         (when-not (= a b)
           (magnitude (sum a b))))
       (filter some?)
       (apply max)))

(defn run [& _]
  (println "Day 18 - Snailfish")
  (let [input (read-input)]
    (println "  part one:" (part-1 input))   ; 3763
    (println "  part two:" (part-2 input)))) ; 4664
