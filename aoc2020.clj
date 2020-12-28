(ns day1
  (:require [clojure.string :as str]))

(defn read-report
  "Naively assuming that report contains only unique entries and
  accumulating those into `hash-set` for fast lookup.

  A possible workaround would be to use `frequences` function, and
  later check if there were more than one entry."
  [input]
  (->> input
       slurp
       str/split-lines
       (map #(Integer. %))
       (into #{})))

(defn- fix-report-two-nums
  "Repeatedly checks if the result of subtracting current item
  from `2020` is available in the set.

  Second optional parameter is used to search for combination of three
  numbers."
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
  (let [report (read-report "inputs/day1.txt")]
    (println "  part one:" (fix-report-two-nums report))     ;; 751776
    (println "  part two:" (fix-report-three-nums report)))) ;; 42275090


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
  (let [password-spec (read-password-spec "inputs/day2.txt")]
    (println "  part one:" (count-old-valid-passwords password-spec))   ;; 434
    (println "  part two:" (count-new-valid-passwords password-spec)))) ;; 509


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
  (let [map-data (read-tree-map "inputs/day3.txt")]
    (println "  part one:" (traverse 3 1 map-data))       ;; 237
    (println "  part two:" (* (traverse 1 1 map-data)
                              (traverse 3 1 map-data)
                              (traverse 5 1 map-data)
                              (traverse 7 1 map-data)
                              (traverse 1 2 map-data))))) ;; 2106818610


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
  (let [passport-specs (read-passport-data "inputs/day4.txt")]
    (println "  part one:" (count-passports-w-all-fields passport-specs)) ;; 196
    (println "  part two:" (count-valid-passports passport-specs))))      ;; 114


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
  (let [boardings (read-boarding-list "inputs/day5.txt")
        seat-ids (map calc-id boardings)]
    (println "  part one:" (apply max seat-ids))  ;; 908
    (println "  part two: " (my-seat seat-ids)))) ;; 619


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
  (let [answers (read-answers "inputs/day6.txt")]
    (println "  part one:" (count-group-answers answers))          ;; 7027
    (println "  part two:" (count-simultaneous-answers answers)))) ;; 3579


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

(defn- count-inner [bags color]
  (let [inner-bags (bags color)]
    (if (empty? inner-bags) 1
        (reduce + 1 (map (fn [[color amount]]
                           (* amount (count-inner bags color))) inner-bags)))))

(defn handy-haversacks []
  (println "Day 7 - Handy Haversacks")
  (let [bags (read-bag-info "inputs/day7.txt")]
    (println "  part one:" (count-bags bags "shiny gold"))          ;; 128
    (println "  part two:" (dec (count-inner bags "shiny gold"))))) ;; 20189


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

(defn handheld-halting []
  (println "Day 8 - Handheld Halting")
  (let [programm (read-opcodes "inputs/day8.txt")]
    (println "  part one:" (second (detect-loop programm))) ;; 1709
    (println "  part two:" (fix-boot programm))))           ;; 1976


(ns day9
  (:require [clojure.string :as str]))

(defn- read-data [data]
  (->> data
       slurp
       str/split-lines
       (map #(BigInteger. %))))

(defn- find-sum [data x]
  (or (first (filter #(= x %) (map #(+ (first data) %) (rest data))))
      (and data (find-sum (next data) x))))

(defn- find-weakness [preamble-size data]
  (let [preamble (take preamble-size data)
        data (drop preamble-size data)]
    (loop [preamble preamble
           data data]
      (if (find-sum preamble (first data))
        (recur (conj (into [] (drop 1 preamble)) (first data))
               (drop 1 data))
        (first data)))))

(defn- crack-xmas [weakness data]
  (when data
    (let [[state range] (reduce (fn [[_ range] n]
                                  (let [sum (apply +' n range)]
                                    (cond (= sum weakness) (reduced [:found range])
                                          (> sum weakness) (reduced [:_ range])
                                          :else [:_ (conj range n)])))
                                [:_ []] data)]
      (if (= state :found)
        (+' (apply min range) (apply max range))
        (recur weakness (next data))))))

(defn encoding-error []
  (println "Day 9 - Encoding Error")
  (let [data (read-data "inputs/day9.txt")
        weakness (find-weakness 25 data)]
    (println "  part one:" weakness)                           ;; 14144619
    (println "  part two:" (str (crack-xmas weakness data))))) ;; 1766397


(ns day10
  (:require [clojure.string :as str]))

(defn- read-jolt-adapters [input]
  (->> input
       slurp
       str/split-lines
       (map #(Integer. %))
       (into (sorted-set))))

(defn- find-adapter [max-jolts adapters]
  (first (filter #(<= % max-jolts) adapters)))

(defn- connect-adapters [adapters]
  (loop [jolts 0
         [one-jolts three-jolts] [0 0]
         adapters adapters
         old 0]
    (if-let [adapter (find-adapter (+ jolts 3) adapters)]
      (recur (+ jolts adapter)
             (condp = (- adapter old)
               1 [(inc one-jolts) three-jolts]
               3 [one-jolts (inc three-jolts)]
               [one-jolts three-jolts])
             (disj adapters adapter)
             adapter)
      (* one-jolts (inc three-jolts)))))

(defn- combinations [x]
  (- (reduce * (repeat (- x 2) 2))
     (if (> x 4)
       (/ (* (inc (- x 4)) (- x 4)) 2)
       0)))

(defn- adapter-combinations [jolts]
  (->> (conj jolts 0)
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       (partition-by #(= 1 %))
       (filter (fn [x] (every? #(= % 1) x)))
       (map (comp combinations inc #(reduce + %)))
       (reduce *)))

(defn adapter-array []
  (println "Day 10 - Adapter Array")
  (let [adapters (read-jolt-adapters "inputs/day10.txt")]
    (println "  part one:" (connect-adapters adapters))       ;; 2400
    (println "  part two:" (adapter-combinations adapters)))) ;; 338510590509056


(ns day11
  (:require [clojure.string :as str]))

(defn- read-seat-data [input]
  (->> input
       slurp
       str/split-lines
       (map #(into [] (str/split % #"")))
       (into [])))

(defn- occupied? [seat]
  (if (and seat (= seat "#")) 1 0))

(defn- seat-liberties [x y seats]
  (+ (occupied? (get-in seats [(inc x) y]))
     (occupied? (get-in seats [(dec x) y]))
     (occupied? (get-in seats [x (inc y)]))
     (occupied? (get-in seats [x (dec y)]))
     (occupied? (get-in seats [(inc x) (inc y)]))
     (occupied? (get-in seats [(dec x) (dec y)]))
     (occupied? (get-in seats [(dec x) (inc y)]))
     (occupied? (get-in seats [(inc x) (dec y)]))))

(defn- can-occupy? [seat liberty]
  (and (= seat "L") (= liberty 0)))

(defn- should-leave? [seat liberty max-liberty]
  (and (= seat "#") (>= liberty max-liberty)))

(defn- occupy-seats [seats]
  (let [new-state (atom seats)]
    (dotimes [x (count seats)]
      (dotimes [y (count (first seats))]
        (let [seat (get-in seats [x y])
              liberty (seat-liberties x y seats)]
          (swap! new-state assoc-in [x y]
                 (cond
                   (should-leave? seat liberty 4) "L"
                   (can-occupy? seat liberty) "#"
                   :else seat)))))
    @new-state))


(defn- wide-occupied? [x y dir-x dir-y seats]
  (loop [x (+ dir-x x)
         y (+ dir-y y)]
    (if-let [seat (get-in seats [x y])]
      (cond
        (= "#" seat) 1
        (= "L" seat) 0
        :else (recur (+ x dir-x) (+ y dir-y)))
      0)))

(defn- seat-wide-liberties [x y seats]
  (+ (wide-occupied? x y  1  0 seats)
     (wide-occupied? x y -1  0 seats)
     (wide-occupied? x y  0  1 seats)
     (wide-occupied? x y  0 -1 seats)
     (wide-occupied? x y  1  1 seats)
     (wide-occupied? x y -1 -1 seats)
     (wide-occupied? x y -1  1 seats)
     (wide-occupied? x y  1 -1 seats)))

(defn- wide-occupy-seats [seats]
  (let [new-state (atom seats)]
    (dotimes [x (count seats)]
      (dotimes [y (count (first seats))]
        (let [seat (get-in seats [x y])
              liberty (seat-wide-liberties x y seats)]
          (swap! new-state assoc-in [x y]
                 (cond
                   (should-leave? seat liberty 5) "L"
                   (can-occupy? seat liberty) "#"
                   :else seat)))))
    @new-state))

(defn- simulate-crowd [seats occupy-fn]
  (let [new-state (atom (occupy-seats seats))
        seats (atom seats)]
    (while (not= @new-state @seats)
      (reset! seats @new-state)
      (reset! new-state (occupy-fn @new-state)))
    (count (mapcat (fn [row] (filter #(= "#" %) row)) @new-state))))

(defn seating-system []
  (println "Day 11 - Seating System")
  (let [seats (read-seat-data "inputs/day11.txt")]
    (println "  part one:" (simulate-crowd seats occupy-seats))        ;; 2324
    (println "  part two:" (simulate-crowd seats wide-occupy-seats)))) ;; 2068


(ns day12
  (:require [clojure.string :as str]))

(defn- read-directions
  "Transform 7-directional coordinates into 5-directional coordinates.
  Positive West coordinates become negative East coordinates.
  Positive South coordinates become negative North coordinates.
  Positive Left rotations become negative Right rotations.

      N 1
      |
  W --+-- E
  -1  |   1
      S -1"
  [input]
  (->> input
       slurp
       str/split-lines
       (map #(let [[_ dir amount] (re-find #"([LRFSEWN])(\d+)" %)]
               [(keyword (str/lower-case (cond (= dir "W") "E"
                                               (= dir "S") "N"
                                               (= dir "L") "R"
                                               :else dir)))
                ((if (or (= dir "S") (= dir "W") (= dir "L")) - +) (Integer. amount))]))))

(defn- move-ship [{:keys [n e angle] :as ship} [direction & directions]]
  (if-let [[direction amount] direction]
    (recur (case direction
             (:n :e) (update ship direction #(+ amount %))
             :r (update ship :angle #(mod (+ % amount) 360))
             :f (condp = angle
                  0   (update ship :n #(+ % amount))
                  90  (update ship :e #(+ % amount))
                  180 (update ship :n #(- % amount))
                  270 (update ship :e #(- % amount))))
           directions)
    (+ (Math/abs n) (Math/abs e))))

(defn- move-ship-to-waypoint [{:keys [n e wn we] :as ship} [direction & directions]]
    (if-let [[direction amount] direction]
      (recur (case direction
               :n (assoc ship :wn (+ amount wn))
               :e (assoc ship :we (+ amount we))
               (:r :l) (condp = (mod amount 360)
                         0   ship
                         90  (assoc ship :wn (- we) :we wn)
                         180 (assoc ship :wn (- wn) :we (- we))
                         270 (assoc ship :wn we :we (- wn)))
               :f (assoc ship :n (+ (* wn amount) n) :e (+ (* we amount) e)))
             directions)
      (+ (Math/abs n) (Math/abs e))))

(defn rain-risk []
  (println "Day 12 - Rain Risk")
  (let [directions (read-directions "inputs/day12.txt")]
    (println "  part one:" (move-ship {:n 0 :e 0 :angle 90} directions))                  ;; 879
    (println "  part two:" (move-ship-to-waypoint {:n 0 :e 0 :wn 1 :we 10} directions)))) ;; 18107


(ns day13
  (:require [clojure.string :as str]))

(defn- read-schedule [input]
  (let [[estimate ids] (-> input
                           slurp
                           (str/split-lines))]
    {:estimate (Integer. estimate)
     :ids (->> (str/split ids #",")
               (map #(if (re-find #"\d+" %) (Integer. %) (keyword %))))}))

(defn- earliest-bus [{:keys [estimate ids]}]
  (let [[id times] (->> ids
                        (filter pos-int?)
                        (map #(do [% (int (/ estimate %))]))
                        (sort (fn [[_ a] [_ b]] (< a b)))
                        first)
        timestamp (if (< (* id times) estimate)
                    (* id (inc times))
                    (* id times))]
    (* (- timestamp estimate) id)))

(defn- continious-bus-departures [{:keys [ids]}]
  (let [time-slots (->> ids
                        (map-indexed vector)
                        (filter (fn [[_ id]] (pos-int? id))))
        #_#_ id (second (first time-slots))]
    ;; Works for examples, but too slow for the real input
    #_(loop [timestamp id]
        (if (every? (fn [[delta id]]
                      (zero? (mod (+ timestamp delta) id)))
                    time-slots)
          timestamp
          (recur (+ timestamp id))))
    (->> time-slots
         (reduce (fn [[timestamp next-time] [delta id]]
                   (if (zero? (mod (+ timestamp delta) id))
                     [timestamp (* next-time id)]
                     (recur [(+ timestamp next-time) next-time]
                            [delta id]))))
         first)))

(defn shuttle-search []
  (println "Day 13 - Shuttle Search")
  (let [schedule (read-schedule "inputs/day13.txt")]
    (println "  part one:" (earliest-bus schedule))                ;; 6559
    (println "  part two:" (continious-bus-departures schedule)))) ;; 626670513163231


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

(defn docking-data []
  (println "Day 14 - Docking Data")
  (let [data (read-data "inputs/day14.txt")]
    (println "  part one:" (write-to-mem data)) ;; 10452688630537
    (println "  part two:" (write-to-mem-v2 data)))) ;; 2881082759597


(ns aoc2020
  (:require [day1] [day2] [day3] [day4] [day5]
            [day6] [day7] [day8] [day9] [day10]
            [day11] [day12] [day13] [day14]))

(day1/report-repair)
(day2/password-philosophy)
(day3/toboggan-trajectory)
(day4/passport-processing)
(day5/binary-boarding)
(day6/custom-customs)
(day7/handy-haversacks)
(day8/handheld-halting)
(day9/encoding-error)
(day10/adapter-array)
(day11/seating-system)
(day12/rain-risk)
(day13/shuttle-search)
(day14/docking-data)
