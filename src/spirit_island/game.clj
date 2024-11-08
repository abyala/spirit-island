(ns spirit-island.game
  (:require [spirit_island.core :refer [count-when in? map-values rounded-percent average]]
            [spirit_island.metadata :as m]))

(defn random-aspect [spirit]
  (let [aspect (-> (:aspects spirit) (conj nil) shuffle first)]
    (cond-> {:spirit (:name spirit)}
            (some? aspect) (assoc :aspect aspect))))

(defn random-game [metadata-svc players]
  {:adversaries {(first (shuffle (m/adversary-names metadata-svc))) (inc (rand-int 6))}
   :players     (zipmap players (map (fn [spirit board] (assoc (random-aspect spirit) :board board))
                                     (shuffle (m/spirits-and-aspects metadata-svc))
                                     (shuffle (m/all-boards metadata-svc))))})

(defn adversaries-in-game [game] (keys (:adversaries game)))
(defn players-in-game [game] (keys (:players game)))
(defn spirits-in-game [game] (map (comp :spirit second) (:players game)))

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
                      (cond-> {:num      num,
                               :wins     wins,
                               :losses   (- num wins),
                               :win-rate (rounded-percent wins num)}
                              (some? rating) (assoc :rating rating)))))

(def #^{:private true} win-rate-comparator (juxt (comp - :win-rate second)
                                                 (comp - :num second)
                                                 first))

(defn stats-by-adversary [games]
  (->> games
       (group-by (comp first keys :adversaries))
       (map-values game-stats)
       (sort-by win-rate-comparator)))

(defn stats-by-spirit
  ([metadata-svc games] (stats-by-spirit metadata-svc games false))
  ([metadata-svc games include-all-spirits?]
   (let [stats (->> games
                    (mapcat (fn [g] (map #(merge {:outcome (:outcome g)} %) ((comp vals :players) g))))
                    (group-by :spirit)
                    (reduce-kv (fn [acc spirit games] (assoc acc spirit (game-stats games))) {})
                    (sort-by (comp :rating second)))
         stats' (if include-all-spirits? (merge (zipmap (m/spirit-ids metadata-svc) (repeat {:num 0 :wins 0 :losses 0}))
                                                stats)
                                         stats)]
     (sort-by (comp :rating second) stats'))))
