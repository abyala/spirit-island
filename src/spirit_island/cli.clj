(ns spirit-island.cli
  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [spirit-island.cli-parse :as clip]
            [spirit_island.core :refer [only-when first-some? say]]
            [spirit-island.game :as g]
            [spirit_island.metadata :as m]
            [spirit-island.users :as u]))

(defn- state-metadata [state] (:metadata state))
(defn- state-users [state] (:users state))

(def ditto "\"")
(def #^{:private true} create-game-usage
  "create-game [Player;...]")
(def #^{:private true} record-game-usage
  "record-game [win|loss] [num-turns] [none|adversary=level] [Player=spirit,(aspect?,)board,rating;...]")
(def #^{:private true} stats-usage
  "stats (adversary|player(=spirit);... )")
(defn invalid-format-message [message]
  (println "Invalid format:" message))

(defn valid-state? [s]
  (every? some? ((juxt :metadata :users) s)))

(defmulti execute (fn [_ line] (when (not= "" (or line ""))
                                 (-> line (str/split #" ") first))))

(defmethod execute :default [_ _]
  (println "Unrecognized command. Use one of the following:")
  (println (str/join "\n" (map #(str " * " %) ["spirits" "users" create-game-usage record-game-usage stats-usage]))))

(defmethod execute nil [_ _])
(defmethod execute "exit" [_ _] :exit)
(defmethod execute "users" [state _]
  (println "Listing all of the users:")
  (doseq [user (sort (u/all-users (state-users state)))]
    (println user)))

(defmethod execute "spirits" [state _]
  (println "Listing all of the spirits:")
  (doseq [spirit (sort (m/spirit-names (state-metadata state)))]
    (println spirit)))

(defn print-game [game]
  (let [{:keys [adversaries players]} game]
    (println "Adversaries:" (if (empty? adversaries)
                              "None"
                              (str/join ", " (map (fn [[k v]] (str (str/capitalize (subs (str k) 1))
                                                                   " level " v)) adversaries))))
    (println "Players:")
    (doseq [[name {spirit :spirit, aspect :aspect, board :board}] players]
      (println (str " * " name
                    " is using board \"" (subs (str board) 1)
                    "\" to play \"" spirit "\""
                    (when aspect (str " with aspect \"" aspect "\"")))))))

(defmethod execute "create-game" [state input]
  (let [players (clip/parse-create-game input)]
    (if (= players :invalid)
      (invalid-format-message create-game-usage)
      (if (not (u/player? (state-users state) players))
        (println "Invalid players")
        (do (print-game (g/random-game (state-metadata state) players))
            :preserve-request)))))

(defmethod execute "record-game" [state input]
  (let [metadata (state-metadata state)
        users (state-users state)
        game (clip/parse-record-game input)
        error (fn [m] (println m) (invalid-format-message record-game-usage) state)]
    (cond (keyword? game) (error game)
          (not (every? (partial m/adversary? metadata) (-> game :adversaries keys))) (error :invalid-adversary)
          (not (every? (partial u/player? users) (-> game :players keys))) (error :invalid-player)
          (not (every? (fn [[s a]] (m/spirit? metadata s a))
                       (->> game :players vals (map (juxt :spirit :aspect))))) (error :invalid-spirit)
          (not (every? (partial m/board? metadata) (->> game :players vals (map :board)))) (error :invalid-board)
          :else (let [{:keys [win? turns adversaries players]} game
                      users' (u/add-game (state-users state) win? turns adversaries players)]
                  (println "Recorded and saved")
                  (assoc state :users users')))))

(defn parse-stat-filters [state input]
  (if (<= (count input) 6)
    ()
    (let [metadata (state-metadata state)
          users (state-users state)]
      (letfn [(parsed-adversary [s] (when (m/adversary? metadata s)
                                      #(g/against-adversary? % (keyword s))))
              (parsed-user [s] (when (u/player? users s)
                                 #(g/with-player? % s)))
              (parsed-user-spirit [s] (when-some [[_ player spirit] (re-matches #"(\w+)=([\w-]+)" s)]
                                        (when (and (u/player? users player) (m/spirit? metadata spirit))
                                          #(g/with-player-and-spirit? % player (keyword spirit)))))]
        (->> (str/split (subs input 6) #";")
             (map #(first-some? ((juxt parsed-adversary parsed-user parsed-user-spirit) %)))
             (only-when #(not-any? nil? %)))))))

(defn filtered-games [state input]
  (if-some [filters (parse-stat-filters state input)]
    (reduce #(filter %2 %1) (u/all-games (state-users state)) filters)
    :invalid))

(defn one-decimal [n] (format "%.1f" (float n)))

(defn print-stats [stats]
  (letfn [(format-rating [rating] (when rating (str " and a rating of " (one-decimal rating))))
          (format-stats [{:keys [num wins win-rate rating]}] (str wins (say wins " win" " wins")
                                                           " out of " num (say num " game" " games")
                                                           " for a win rate of " win-rate "%"
                                                           (format-rating rating)))]
    (cond (map? stats) (println (format-stats stats))
          (map? (-> stats first second)) (doseq [[id s] stats] (println (str "For " (str id) ", " (format-stats s))))
          :else (assert "Unknown datatype for stats:" (type stats)))))

(defn show-all-stats [games]
  (letfn [(find-and-print [title lookup-fn]
            (println title)
            (print-stats (lookup-fn games))
            (println))]
    (if (seq games)
      (do (find-and-print "Summary details:" g/game-stats)
          (find-and-print "Stats by adversary:" g/stats-by-adversary)
          (find-and-print "Stats by spirit:" g/stats-by-spirit))
      (println "No matching games found"))))

(defmethod execute "stats" [state input]
  (let [games (filtered-games state input)]
    (if (= games :invalid)
      (invalid-format-message stats-usage)
      (show-all-stats games))))

(defn run-cli
  ([] (run-cli (spirit_island.metadata/parse-metadata) (spirit-island.users/parse-users)))
  ([state]
   (println)
   (let [remove-previous (fn [s] (dissoc s :previous-request))
         line-orig (str/trim (read-line))
         line (condp = line-orig
                "" nil
                ditto (:previous-request state)
                line-orig)
         response (execute state line)]
     (cond
       (nil? line)                    (recur state)
       (= :exit response)             (println "Exiting")
       (nil? response)                (recur (remove-previous state))
       (= :preserve-request response) (recur (assoc state :previous-request line))
       (valid-state? response)        (recur (remove-previous response))
       :else (println "ERROR: Unexpected response in CLI.  Exiting to be safe."))))
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