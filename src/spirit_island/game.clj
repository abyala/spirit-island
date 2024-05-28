(ns spirit-island.game
  (:require [spirit_island.core :refer [in? map-values]]
            [spirit_island.metadata :as m])
  (:import [java.util Random]))

(defn random-game [metadata players]
  {:adversaries {(first (shuffle (m/adversary-names metadata))) (-> (Random.) (.nextInt 6) inc)}
   :players     (zipmap players (map #(hash-map :spirit %1 :board %2)
                                     (shuffle (m/spirit-names metadata))
                                     (shuffle (m/all-boards metadata))))})

(defn- adversaries-in-game [game] (keys (:adversaries game)))
(defn- players-in-game [game] (keys (:players game)))
(defn- spirits-in-game [game] (map (comp :spirit second) (:players game)))

(defn against-adversary? [game adversary] (in? (adversaries-in-game game) adversary))
(defn with-player? [game player] (in? (players-in-game game) player))
(defn with-spirit? [game spirit] (in? (spirits-in-game game) spirit))
(defn with-player-and-spirit? [game player spirit] (some (fn [[p {s :spirit}]] (and (= p player) (= s spirit)))
                                                         (:players game)))

(defn game-stats [games]
  (when (seq games) (let [n (count games)
                        wins (count (filter #(-> % :outcome (= :win)) games))]
                    {:num (count games) :win-rate (Math/round ^Double (* 100 (/ wins n)))})))

(def #^{:private true} win-rate-comparator (juxt (comp - :win-rate second)
                                                 (comp - :num second)
                                                 first))

(defn stats-by-adversary [games]
  (->> games
       (group-by (comp first keys :adversaries))
       (map-values game-stats)
       (sort-by win-rate-comparator)))
