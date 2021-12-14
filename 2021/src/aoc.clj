(ns aoc
  (:gen-class))

(defn run-task [day]
  (try
    (let [ns (symbol (str "day" day))]
      (require ns)
      ((ns-resolve ns 'run)))
    (catch Exception _
      (println (str "Skipping day " day)))))

(defn -main [& _]
  (doseq [day (range 1 (inc 14))]
    (run-task day)))
