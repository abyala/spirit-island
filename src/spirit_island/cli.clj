(ns spirit-island.cli
  (:require [clojure.string :as str]
            [integrant.core :as ig]
            [spirit_island.core :as core :refer [only-when parse-long-within-range]]
            [spirit_island.metadata :as m]
            [spirit-island.users :as u]))


(defn- state-metadata [state] (:metadata state))
(defn- state-users [state] (:users state))

(defmulti execute (fn [_ line] (-> line (str/split #" ") first)))

(defmethod execute :default [state _]
  (println "Unrecognized command. Use one of the following:\n * spirits\n * users\n * create-game\n * record-game\n * exit")
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
      (println "Invalid players:" players)
      (print-game (core/random-game (state-metadata state) players)))
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
              (reduce (fn [acc s] (if-some [[_ player spirit rating] (re-matches #"(\w+)=([\w-]+),(\d+)" s)]
                                    (if (and (u/valid? (state-users state) [player])
                                             (m/spirit? (state-metadata state) spirit)
                                             (parse-long-within-range rating 1 5))
                                      (assoc acc player {:spirit (keyword spirit) :rating (parse-long rating)})
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
    (do (println "Invalid game structure.  Format: [win|loss] [num-turns] [adversary=rating] [Player=spirit,rating;...]")
        state)))

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