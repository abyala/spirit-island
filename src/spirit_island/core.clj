(ns spirit_island.core)

(defn only-when [f v]
  (when (f v) v))

(defn numeric? [s] (try (some? (parse-long s))
                        (catch IllegalArgumentException _ false)))

(defn parse-long-within-range [s low high]
  (when (numeric? s)
    (only-when #(<= low % high) (parse-long s))))

(defn in? [coll v] (some #(= v %) coll))
