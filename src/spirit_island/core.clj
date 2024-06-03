(ns spirit_island.core)

(defn only-when
  "Returns v if (f v) is truthy, or else returns nil."
  [f v]
  (when (f v) v))

(defn numeric? [s] (try (some? (parse-long s))
                        (catch IllegalArgumentException _ false)))

(defn parse-long-within-range [s low high]
  (when (numeric? s)
    (only-when #(<= low % high) (parse-long s))))

(defn in? [coll v] (some #(= v %) coll))

(defn map-values [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn first-when [f coll]
  (first (filter f coll)))

(defn first-some? [coll] (first-when some? coll))

(defn say
  "Returns the `singular` string if n = 1, or else the `multiple` string. Helpful for forming proper English sentences."
  [n singular multiple] (if (= n 1) singular multiple))

(defn no-nil-vals
  "Returns the input map without any key whose value was nil."
  [m]
  (when m (reduce-kv #(if (some? %3) (assoc %1 %2 %3) %1) {} m)))