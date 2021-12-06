(ns day14
  (:require [clojure.string :as str]))

(defn- read-data [input]
  (->> input
       slurp
       str/split-lines
       (map #(cond (str/starts-with? % "mask")
                   (let [[_ mask] (re-find #"^mask = (.*)" %)]
                     [:mask mask])
                   (str/starts-with? % "mem")
                   (let [[_ addr val] (re-find #"^mem\[(\d+)\] = (\d+)" %)]
                     [(Integer. addr) (Integer. val)])))))

(defn- make-mask [mask]
  (let [and-mask (-> mask
                     (str/replace #"[01]" "0")
                     (str/replace #"[X]" "1")
                     (Long/parseLong 2))
        or-mask (-> mask
                    (str/replace #"[X]" "0")
                    (Long/parseLong 2))]
    (fn [n] (-> n (bit-and and-mask) (bit-or or-mask)))))

(defn- write-to-mem [data]
  (let [mask-fn (volatile! nil)]
    (loop [[[instr instr-data] & data] data
           mem {}]
      (if instr
        (case instr
          :mask (do (vreset! mask-fn (make-mask instr-data))
                    (recur data mem))
          (recur data
                 (assoc mem instr (@mask-fn instr-data))))
        (apply + (vals mem))))))

(defn- apply-bitmask
  "Applies `mask` with \"X\" replaced out to the `addr`, and produces 36
  bit `Long` as a `String` with \"X\" at original positions.

  addr    000000000000000000000000000000101010 (decimal 42)
  mask:   000000000000000000000000000000X1001X
  result: 000000000000000000000000000000X1101X
  "
  [mask addr]
  (let [xs (disj (->> mask
                         (map-indexed vector)
                         (filter (fn [[_ c]] (= c \X)))
                         flatten
                         (into #{})) \X)
        mask (Long/parseLong (str/replace mask #"[X]" "1") 2)
        mask (->> (Long/toBinaryString (bit-or mask addr))
                  (str "000000000000000000000000000000000000000000000000000000000000000"))
        mask (drop (- (count mask) 36) mask)]
    (loop [mask (into [] mask)
           [i & xs] xs]
      (if i
        (recur (assoc mask i \X) xs)
        (str/join mask)))))

(defn- gen-adresses
  "Generate all variations of adresses by recursively replacing \"X\"
  with `1` and `0`."
  ([mask]
   (-> (gen-adresses mask nil) flatten distinct))
  ([mask _]
   (let [a (str/replace-first mask #"[X]" "0")
         b (str/replace-first mask #"[X]" "1")]
     (if (re-find #"X" (str/replace-first mask #"[X]" "0"))
       [(gen-adresses a nil) (gen-adresses b nil)]
       [(Long/parseLong (str/replace a #"[X]" "0") 2)
        (Long/parseLong (str/replace b #"[X]" "0") 2)]))))

(defn- write-to-mem-v2 [data]
  (loop [[[instr instr-data] & data] data
         mask nil
         mem {}]
    (if instr
      (case instr
        :mask (recur data instr-data mem)
        (recur data
               mask
               (apply assoc mem (interleave (gen-adresses (apply-bitmask mask instr)) (repeat instr-data)))))
      (apply + (vals mem)))))

(defn run [& _]
  (println "Day 14 - Docking Data")
  (let [data (read-data "inputs/day14.txt")]
    (println "  part one:" (write-to-mem data))      ; 10452688630537
    (println "  part two:" (write-to-mem-v2 data)))) ; 2881082759597
