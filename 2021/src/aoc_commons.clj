(ns aoc-commons)

(defn parse-long [s]
  (if (or (char? s) (string? s))
    (try
      (Long/valueOf (str s))
      (catch NumberFormatException _
        nil))
    (throw (IllegalArgumentException.
            (format "Expected string, got %s" (some-> s class .getName))))))
