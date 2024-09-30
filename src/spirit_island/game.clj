(ns spirit-island.game
  (:require [spirit_island.core :refer [count-when in? map-values rounded-percent average no-nil-vals]]
            [spirit_island.metadata :as m]))

(defn random-aspect [spirit]
  (let [aspects (:aspects spirit)
        spirit' {:spirit (:name spirit)}]
    (if-not aspects
      spirit'
      (no-nil-vals (assoc spirit' :aspect (first (shuffle (conj aspects nil))))))))

(defn random-game [metadata players]
  {:adversaries {(first (shuffle (m/adversary-names metadata))) (inc (rand-int 6))}
   :players     (zipmap players (map (fn [spirit board] (assoc (random-aspect spirit) :board board))
                                     (shuffle (m/spirits-and-aspects metadata))
                                     (shuffle (m/all-boards metadata))))})

(defn- adversaries-in-game [game] (keys (:adversaries game)))
(defn- players-in-game [game] (keys (:players game)))
(defn- spirits-in-game [game] (map (comp :spirit second) (:players game)))

(defn against-adversary? [game adversary] (in? (adversaries-in-game game) adversary))
(defn with-player? [game player] (in? (players-in-game game) player))
(defn with-spirit? [game spirit] (in? (spirits-in-game game) spirit))
(defn with-player-and-spirit? [game player spirit] (some (fn [[p {s :spirit}]] (and (= p player) (= s spirit)))
                                                         (:players game)))
(defn win? [game] (= :win (:outcome game)))

(defn game-stats [games]
  (when (seq games) (let [num (count games)
                          wins (count-when win? games)
                          rating (when (:rating (first games))
                                   (* 1.0 (average (map :rating games))))]
                      (no-nil-vals {:num      num,
                                    :wins     wins,
                                    :losses   (- num wins),
                                    :win-rate (rounded-percent wins num)
                                    :rating   rating}))))

(def #^{:private true} win-rate-comparator (juxt (comp - :win-rate second)
                                                 (comp - :num second)
                                                 first))

(defn stats-by-adversary [games]
  (->> games
       (group-by (comp first keys :adversaries))
       (map-values game-stats)
       (sort-by win-rate-comparator)))

(defn stats-by-spirit
  ([games] (->> games
                (mapcat (fn [g] (map #(merge {:outcome (:outcome g)} %) ((comp vals :players) g))))
                (group-by :spirit)
                (reduce-kv (fn [acc spirit games] (assoc acc spirit (game-stats games))) {})
                (sort-by (comp :rating second))))
  ([games player] (stats-by-spirit (map #(update % :players select-keys [player]) games))))
