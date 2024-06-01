(ns spirit-island.cli
  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [spirit_island.core :refer [only-when parse-long-within-range first-some? say]]
            [spirit-island.game :as g]
            [spirit_island.metadata :as m]
            [spirit-island.users :as u]))

(defn- state-metadata [state] (:metadata state))
(defn- state-users [state] (:users state))

(def #^{:private true} create-game-usage
  "create-game [Player;...]")
(def #^{:private true} record-game-usage
  "record-game [win|loss] [num-turns] [none|adversary=level] [Player=spirit,board,rating;...]")
(def #^{:private true} stats-usage
  "stats (adversary|player(=spirit);... )")
(defn invalid-format-message [message]
  (println "Invalid format:" message))

(defmulti execute (fn [_ line] (-> line (str/split #" ") first)))

(defmethod execute :default [state _]
  (println "Unrecognized command. Use one of the following:")
  (println (str/join "\n" (map #(str " * " %) ["spirits" "users" create-game-usage record-game-usage stats-usage])))
  state)

(defmethod execute "exit" [_ _])
(defmethod execute "users" [state _]
  (println "Listing all of the users:")
  (doseq [user (sort (u/all-users (state-users state)))]
    (println user))
  state)

(defmethod execute "spirits" [state _]
  (println "Listing all of the spirits:")
  (doseq [spirit (sort (m/spirit-names (state-metadata state)))]
    (println spirit))
  state)

(defn print-game [game]
  (let [{:keys [adversaries players]} game]
    (println "Adversaries:" (if (empty? adversaries)
                              "None"
                              (str/join ", " (map (fn [[k v]] (str (str/capitalize (subs (str k) 1))
                                                                   " level " v)) adversaries))))
    (println "Players:")
    (doseq [[name {spirit :spirit, board :board}] players]
      (println (str " * " name " is using board \"" (subs (str board) 1) "\" to play \"" spirit "\"")))))

(defmethod execute "create-game" [state input]
  (let [[_ players-str] (str/split input #" ")
        players (str/split (or players-str "") #";")]
    (if (not (u/valid? (state-users state) players))
      (invalid-format-message create-game-usage)
      (print-game (g/random-game (state-metadata state) players)))
    state))

(defn parse-record-game [state input]
  (letfn [(parse-adversaries [input-str]
            (case input-str nil nil
                            "none" {}
                            ":none" {}
                            (reduce (fn [acc s] (if-some [[_ adversary level] (re-matches #"(\w+)=(\d+)" s)]
                                                  (if (and (m/adversary? (state-metadata state) adversary)
                                                           (parse-long-within-range level 0 6))
                                                    (assoc acc (keyword adversary) (parse-long level))
                                                    (reduced nil))
                                                  (reduced nil)))
                                    {}
                                    (str/split input-str #";"))))
          (parse-players [input-str]
            (when input-str
              (reduce (fn [acc s] (if-some [[_ player spirit board rating] (re-matches #"(\w+)=([\w-]+),(\w),(\d+)" s)]
                                    (if (and (u/valid? (state-users state) player)
                                             (m/spirit? (state-metadata state) spirit)
                                             (m/board? (state-metadata state) board)
                                             (parse-long-within-range rating 1 5))
                                      (assoc acc player {:spirit (keyword spirit)
                                                         :board  (keyword board)
                                                         :rating (parse-long rating)})
                                      (reduced nil))
                                    (reduced nil)))
                      {}
                      (str/split input-str #";"))))]
    (let [[_ win-loss-str turns-str adversaries-str players-str] (str/split input #" ")]
      (only-when #(not-any? nil? (vals %))
                 {:win?        ({"win" :win "loss" :loss} win-loss-str)
                  :turns       (parse-long-within-range turns-str 1 13)
                  :adversaries (parse-adversaries adversaries-str)
                  :players     (parse-players players-str)}))))

(defmethod execute "record-game" [state input]
  (if-some [game (parse-record-game state input)]
    (let [{:keys [win? turns adversaries players]} game
          users' (u/add-game (state-users state) win? turns adversaries players)]
      (println "Recorded and saved")
      (assoc state :users users'))
    (do (invalid-format-message record-game-usage)
        state)))

(defn parse-stat-filters [state input]
  (if (<= (count input) 6)
    ()
    (let [metadata (state-metadata state)
          users (state-users state)]
      (letfn [(parsed-adversary [s] (when (m/adversary? metadata s)
                                      #(g/against-adversary? % (keyword s))))
              (parsed-user [s] (when (u/valid? users s)
                                 #(g/with-player? % s)))
              (parsed-user-spirit [s] (when-some [[_ player spirit] (re-matches #"(\w+)=([\w-]+)" s)]
                                        (when (and (u/valid? users player) (m/spirit? metadata spirit))
                                          #(g/with-player-and-spirit? % player (keyword spirit)))))]
        (->> (str/split (subs input 6) #";")
             (map #(first-some? ((juxt parsed-adversary parsed-user parsed-user-spirit) %)))
             (only-when #(not-any? nil? %)))))))

(defn filtered-games [state input]
  (if-some [filters (parse-stat-filters state input)]
    (reduce #(filter %2 %1) (u/all-games (state-users state)) filters)
    :invalid))

(defn print-stats [stats]
  (letfn [(format-stats [{:keys [num wins win-rate]}] (str wins (say wins " win" " wins")
                                                           " out of " num (say num " game" " games")
                                                           " for a win rate of " win-rate "%"))]
    (cond (map? stats) (println (format-stats stats))
          (map? (-> stats first second)) (doseq [[id s] stats] (println (str "For " (str id) ", " (format-stats s))))
          :else (assert "Unknown datatype for stats:" (type stats)))))

(defn show-all-stats [games]
  (if (seq games)
    (do (println "Summary details:")
        (print-stats (g/game-stats games))
        (println)
        (println "Stats by adversary:")
        (print-stats (g/stats-by-adversary games)))
    (println "No matching games found")))

(defmethod execute "stats" [state input]
  (let [games (filtered-games state input)]
    (if (= games :invalid)
      (invalid-format-message stats-usage)
      (show-all-stats games)))
  state)

(defn run-cli
  ([] (run-cli (spirit_island.metadata/parse-metadata) (spirit-island.users/parse-users)))
  ([state]
   (println)
   (if-let [s' (execute state (read-line))]
     (recur s')
     (println "Exiting")))
  ([metadata users]
   (println "Starting the Spirit Island CLI")
   (run-cli {:metadata metadata :users users})))

(defmethod ig/init-key :spirit-island/cli [_ {:keys [metadata users]}]
  (run-cli metadata users))

(defmethod ig/halt-key! :spirit-island/cli [_ _])

(defn -main [& _]
  (let [config (ig/read-string (slurp "resources/config.edn"))
        system (ig/init config)]
    (ig/halt! system)))