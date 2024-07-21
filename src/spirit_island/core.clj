(ns spirit_island.core)

(defn only-when
  "Returns v if (f v) is truthy, or else returns nil."
  [f v]
  (when (f v) v))

(defn numeric?
  "Returns true if the input value is a numeric string, or else false."
  [s] (try (some? (parse-long s))
           (catch IllegalArgumentException _ false)))

(defn parse-long-within-range
  "Returns the value of a numeric string if it falls within the range of values (inclusive on both sides),
  or else returns `nil`."
  [s low high]
  (when (numeric? s)
    (only-when #(<= low % high) (parse-long s))))

(defn map-values [f m]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(defn in? [coll v] (some (partial = v) coll))
(defn first-when [f coll] (->> coll (filter f) first))
(defn count-when [f coll] (->> coll (filter f) count))
(defn first-some? [coll] (first-when some? coll))

(defn say
  "Returns the `singular` string if n = 1, or else the `multiple` string. Helpful for forming proper English sentences."
  [n singular multiple] (if (= n 1) singular multiple))

(defn no-nil-vals
  "Returns the input map without any key whose value was nil."
  [m]
  (when m (reduce-kv #(if (some? %3) (assoc %1 %2 %3) %1) {} m)))

(defn rounded-percent
  "Returns the value of a numeric fraction as a rounded percent.\n
  The 1-arity function takes the value of the fraction.\n
  The 2-arity function takes the numerator and the denominator."
  ([n] (Math/round ^Double (* 100 n)))
  ([numer denom] (when-not (zero? denom) (rounded-percent (/ numer denom)))))

(defn average [nums]
  "Returns the average of a collection of numbers, or `nil` if the collection is `nil` or empty."
  (when (seq nums)
    (/ (apply + nums) (count nums))))