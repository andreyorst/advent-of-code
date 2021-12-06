(ns aoc-commons)

(defn parse-long [s]
  (if (string? s)
    (try
      (Long/valueOf s)
      (catch NumberFormatException _
        nil))
    (throw (IllegalArgumentException.
            (format "Expected string, got %s" (some-> s class .getName))))))
