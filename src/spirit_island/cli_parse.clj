(ns spirit-island.cli-parse
  (:require [clojure.string :as str]
            [spirit_island.core :refer [first-when only-when parse-long-within-range]]))

(defn parse-create-game [input]
  (let [[_ players-str] (str/split input #" ")]
    (if players-str (str/split players-str #";") :invalid)))

(defn parse-record-game-win? [input] (case input "win" :win "loss" :loss :invalid-win?))
(defn parse-record-game-turns [input] (or (parse-long-within-range input 1 13) :invalid-turns))

(defn parse-record-game-adversaries [input]
  (case input nil :missing-adversary
              "none" {}
              ":none" {}
              (reduce (fn [acc s] (if-some [[_ adversary level-str] (re-matches #"(\w+)=(\d+)" s)]
                                    (if-some [level (parse-long-within-range level-str 0 6)]
                                      (assoc acc (keyword adversary) level)
                                      (reduced :invalid-adversary-level))
                                    (reduced :invalid-adversary)))
                      {}
                      (str/split input #";"))))

(defn parse-record-game-players [input]
  (if (only-when #(not (str/blank? %)) input)
    (reduce (fn [acc s] (let [[name-spirit & tokens] (str/split s #",")
                              [player spirit] (str/split name-spirit #"=")]
                          (if (<= 2 (count tokens) 3)
                            (let [[aspect board rating-str] (if (= 3 (count tokens)) tokens (concat [nil] tokens))
                                  aspect (only-when (partial not= "base") aspect)
                                  rating (parse-long-within-range rating-str 1 5)]
                              (cond (nil? player) (reduced :invalid-player)
                                    (nil? spirit) (reduced :invalid-spirit)
                                    (nil? board) (reduced :invalid-board)
                                    (nil? rating) (reduced :invalid-player-rating)
                                    :else (assoc acc player (cond-> {:spirit (keyword spirit)
                                                                     :board  (keyword board)
                                                                     :rating rating}
                                                                    (some? aspect) (assoc :aspect (keyword aspect))))))
                            (reduced :invalid-player))))
            {}
            (str/split input #";"))
    :missing-players))

(defn error-message? [v] (and (keyword? v) (not (#{:win :loss} v))))
(defn first-error [m] (first-when error-message? (vals m)))

(defn parse-record-game [input]
  (let [[_ win-loss-str turns-str adversaries-str players-str] (str/split input #" ")
        result {:win?        (parse-record-game-win? win-loss-str)
                :turns       (parse-record-game-turns turns-str)
                :adversaries (parse-record-game-adversaries adversaries-str)
                :players     (parse-record-game-players players-str)}]
    (or (first-error result) result)))