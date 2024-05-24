(ns spirit-island.game
  (:require [spirit_island.core :refer [in?]]
            [spirit_island.metadata :as m])
  (:import [java.util Random]))

(defn random-game [metadata players]
  {:adversaries {(first (shuffle (m/adversary-names metadata))) (-> (Random.) (.nextInt 6) inc)}
   :players     (zipmap players (map #(hash-map :spirit %1 :board %2)
                                     (shuffle (m/spirit-names metadata))
                                     (shuffle (m/all-boards metadata))))})

(defn games-against [games adversary]
  (filter #(in? (keys (:adversaries %)) adversary) games))

(defn games-with-player [games player]
  (filter #(in? (keys (:players %)) player) games))

(defn spirits-in-game [game]
  (map (comp :spirit second) (:players game)))

(defn games-with-spirit
  ([games spirit] (filter #(in? (spirits-in-game %) spirit) games))
  ([games spirit player] (filter (fn [game] (some (fn [[p {s :spirit}]] (and (= p player) (= s spirit)))
                                                  (:players game)))
                                 games)))
