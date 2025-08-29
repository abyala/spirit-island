(ns spirit-island.cli
  (:require [clojure.string :as str]
            [clj-time.coerce :as coer]
            [clj-time.format :as fmt]
            [integrant.core :as ig]
            [spirit-island.cli-parse :as clip]
            [spirit_island.core :refer [only-when first-some? say]]
            [spirit-island.game :as g]
            [spirit_island.metadata :as m]
            [spirit-island.users :as u]))

(defn- state-metadata-svc [state] (:metadata-svc state))
(defn- state-user-svc [state] (:user-svc state))

(def ditto "\"")
(def #^{:private true} filters-usage "(adversary|player(=spirit);... )")
(def #^{:private true} create-game-usage
  "create-game [Player;...]")
(def #^{:private true} record-game-usage
  "record-game [win|loss] [num-turns] [none|adversary=level] [Player=spirit,(aspect?,)board,rating;...]")
(def #^{:private true} stats-usage (str "stats " filters-usage))
(def #^{:private true} games-usage (str "games " filters-usage))
(defn invalid-format-message [message]
  (println "Invalid format:" message))

(defn valid-state? [s]
  (every? some? ((juxt :metadata-svc :user-svc) s)))

(defmulti execute (fn [_ line] (when (not= "" (or line ""))
                                 (-> line (str/split #" ") first))))

(defmethod execute :default [_ _]
  (println "Unrecognized command. Use one of the following:")
  (println (str/join "\n" (map #(str " * " %) ["spirits" "users" create-game-usage record-game-usage stats-usage games-usage]))))

(defmethod execute nil [_ _])
(defmethod execute "exit" [_ _] :exit)
(defmethod execute "users" [state _]
  (println "Listing all of the users:")
  (doseq [user (sort (u/all-users (state-user-svc state)))]
    (println user)))

(defmethod execute "spirits" [state _]
  (println "Listing all of the spirits:")
  (doseq [spirit (sort (m/spirit-names (state-metadata-svc state)))]
    (println spirit)))

(defn print-game-setup [game]
  (let [{:keys [adversaries players]} game]
    (println "Adversaries:" (if (empty? adversaries)
                              "None"
                              (str/join ", " (map (fn [[k v]] (str (str/capitalize (subs (str k) 1))
                                                                   " level " v)) adversaries))))
    (println "Players:")
    (doseq [[player {spirit :spirit, aspect :aspect, board :board}] players]
      (println (str " * " player
                    " is using board \"" (name board)
                    "\" to play \"" spirit "\""
                    (when aspect (str " with aspect \"" aspect "\"")))))))

(defmethod execute "create-game" [state input]
  (let [players (clip/parse-create-game input)]
    (if (= players :invalid)
      (invalid-format-message create-game-usage)
      (if (not (u/players? (state-user-svc state) players))
        (println "Invalid players")
        (do (print-game-setup (g/random-game (state-metadata-svc state) players))
            :preserve-request)))))

(defmethod execute "record-game" [state input]
  (let [[metadata-svc user-svc] ((juxt state-metadata-svc state-user-svc) state)
        game (clip/parse-record-game input)
        error (fn [m] (println m) (invalid-format-message record-game-usage) state)]
    (cond (keyword? game) (error game)
          (not (every? (partial m/adversary? metadata-svc) (g/adversaries-in-game game))) (error :invalid-adversary)
          (not (every? (partial u/player? user-svc) (g/players-in-game game))) (error :invalid-player) ; Don't need every?
          (not (every? (fn [[s a]] (m/spirit? metadata-svc s a))
                       (->> game :players vals (map (juxt :spirit :aspect))))) (error :invalid-spirit)
          (not (every? (partial m/board? metadata-svc) (->> game :players vals (map :board)))) (error :invalid-board)
          :else (let [{:keys [win? turns adversaries players]} game]
                  (u/add-game user-svc win? turns adversaries players)
                  (println "Recorded and saved")))))

(defn parse-stat-filters [state input]
  (if (<= (count input) 6)
    ()
    (let [[metadata-svc user-svc] ((juxt state-metadata-svc state-user-svc) state)]
      (letfn [(parsed-adversary [s]
                (when (m/adversary? metadata-svc s)
                  #(g/against-adversary? % (keyword s))))
              (parsed-user [s] (when (u/player? user-svc s)
                                 #(g/with-player? % s)))
              (parsed-user-spirit [s] (when-some [[_ player spirit] (re-matches #"(\w+)=([\w-]+)" s)]
                                        (when (and (u/player? user-svc player) (m/spirit? metadata-svc spirit))
                                          #(g/with-player-and-spirit? % player (keyword spirit)))))]
        (->> (str/split (subs input 6) #";")
             (map #(first-some? ((juxt parsed-adversary parsed-user parsed-user-spirit) %)))
             (only-when #(not-any? nil? %)))))))

(defn filtered-games [state input]
  (if-some [filters (parse-stat-filters state input)]
    (reduce #(filter %2 %1) (u/all-games (state-user-svc state)) filters)
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

(defn show-all-stats [metadata-svc games]
  (letfn [(find-and-print [title lookup-fn]
            (println title)
            (print-stats (lookup-fn games))
            (println))]
    (if (seq games)
      (do (find-and-print "Summary details:" g/game-stats)
          (find-and-print "Stats by adversary:" g/stats-by-adversary)
          (find-and-print "Stats by spirit:" (partial g/stats-by-spirit metadata-svc)))
      (println "No matching games found"))))

(defmethod execute "stats" [state input]
  (let [games (filtered-games state input)]
    (if (= games :invalid)
      (invalid-format-message stats-usage)
      (show-all-stats (state-metadata-svc state) games))))

(defn date-as-string [d]
  (fmt/unparse (fmt/formatter "yyyy-MM-dd") (coer/from-date d)))

(defn print-game-results [metadata-svc games]
  (doseq [game games]
    (println (str (date-as-string (:timestamp game))
                  ": "
                  ({:win "Victory" :loss "Defeat"} (:outcome game))
                  " after " (:num-turns game) " turns"
                  (if-let [a (seq (:adversaries game))]
                    (str " against " (str/join ", " (map (fn [[n l]] (str (m/adversary-name-by-id metadata-svc n) " level " l))
                                                         a)))
                    " with no adversaries")
                  "\n"
                  (str/join "\n" (map (fn [[p {:keys [spirit aspect board]}]]
                                        (str "\t" p " playing \"" (m/spirit-name-by-id metadata-svc spirit) "\""
                                             (when aspect (str " and aspect \"" (m/aspect-name-by-id metadata-svc spirit aspect) "\""))
                                             (when board (str " on board " (name board)))))
                                      (sort-by first (:players game))))))))

(defmethod execute "games" [state input]
  (let [games (filtered-games state input)]
    (if (= games :invalid)
      (invalid-format-message games-usage)
      (print-game-results (state-metadata-svc state) games))))

; TODO: Consider making the CLI itself follow a protocol? Maybe?

(defn run-cli
  ([] (run-cli (m/create-service) (u/create-service)))
  ([state]
   (println)
   (let [store-previous (fn [s req] (assoc s :previous-request req))
         remove-previous (fn [s] (dissoc s :previous-request))
         line-orig (str/trim (read-line))
         line (condp = line-orig
                "" nil
                ditto (:previous-request state)
                line-orig)
         response (execute state line)]
     (cond
       (nil? line) (recur state)
       (= :exit response) (println "Exiting")
       (nil? response) (recur (remove-previous state))
       (= :preserve-request response) (recur (store-previous state line))
       (valid-state? response) (recur (remove-previous response))
       :else (println "ERROR: Unexpected response in CLI.  Exiting to be safe."))))
  ([metadata-svc user-svc]
   (println "Starting the Spirit Island CLI")
   (run-cli {:metadata-svc metadata-svc :user-svc user-svc})))

(defmethod ig/init-key :spirit-island/cli [_ {:keys [metadata-svc user-svc]}]
  (run-cli metadata-svc user-svc))

(defmethod ig/halt-key! :spirit-island/cli [_ _])

(defn -main [& _]
  (let [config (ig/read-string (slurp "resources/config.edn"))
        system (ig/init config)]
    (ig/halt! system)))