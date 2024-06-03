(ns spirit-island.cli-parse
  (:require [clojure.string :as str]
            [spirit_island.core :refer [first-when no-nil-vals only-when parse-long-within-range]]))

(defn parse-create-game [input]
  (let [[_ players-str] (str/split input #" ")]
    (if players-str (str/split players-str #";") :invalid)))

(defn parse-record-game-win? [input] (case input "win" :win "loss" :loss :invalid-win?))
(defn parse-record-game-turns [input] (or (parse-long-within-range input 1 13) :invalid-turns))

(defn parse-record-game-adversaries [input]
  (case input nil :missing-adversary
              "none" {}
              ":none" {}
              (reduce (fn [acc s] (if-some [[_ adversary level] (re-matches #"(\w+)=(\d+)" s)]
                                    (if (parse-long-within-range level 0 6)
                                      (assoc acc (keyword adversary) (parse-long level))
                                      (reduced :invalid-adversary-level))
                                    (reduced :invalid-adversary)))
                      {}
                      (str/split input #";"))))

(defn parse-record-game-players [input]
  (if input
    (reduce (fn [acc s]
              (if-some [[_ player spirit a b c] (first (keep #(re-matches % s)
                                                             [#"(\w+)=([\w-]+),(\w+),(\d+)"
                                                              #"(\w+)=([\w-]+),([\w-]+),(\w+),(\d+)"]))]
                (let [[aspect-str board rating] (if (some? c) [a b c] [nil a b])
                      aspect (only-when #(not= % "base") aspect-str)]
                  (if (parse-long-within-range rating 1 5)
                    (assoc acc player (no-nil-vals {:spirit (keyword spirit)
                                                    :aspect (keyword aspect)
                                                    :board  (keyword board)
                                                    :rating (parse-long rating)}))
                    (reduced :invalid-player-rating)))
                (reduced :invalid-player)))
            {}
            (str/split input #";"))
    :missing-players))

(defn parse-record-game-players [input]
  (if (only-when #(not (str/blank? %)) input)
    (reduce (fn [acc s] (let [[name-spirit & tokens] (str/split s #",")
                              [player spirit] (str/split name-spirit #"=")]
                          (if (<= 2 (count tokens) 3)
                            (let [[aspect board rating-str] (if (= 3 (count tokens)) tokens (concat [nil] tokens))
                                  rating (parse-long-within-range rating-str 1 5)]
                              (cond (nil? player) (reduced :invalid-player)
                                    (nil? spirit) (reduced :invalid-spirit)
                                    (nil? board) (reduced :invalid-board)
                                    (nil? rating) (reduced :invalid-player-rating)
                                    :else (assoc acc player (no-nil-vals {:spirit (keyword spirit)
                                                                          :aspect (when (and aspect (not= aspect "base"))
                                                                                    (keyword aspect))
                                                                          :board  (keyword board)
                                                                          :rating rating}))))
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