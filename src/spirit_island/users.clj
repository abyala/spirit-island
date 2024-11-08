(ns spirit-island.users
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [integrant.core :as ig])
  (:import [java.util Date]))

(s/def :game-player/spirit keyword?)
(s/def :game-player/rating pos-int?)
(s/def :game-player/aspect keyword?)
(s/def :game/timestamp (partial instance? Date))
(s/def :game/outcome #{:win :loss})
(s/def :game/num-turns (s/and pos-int?))
(s/def :game/adversaries (s/map-of keyword? (partial <= 0)))
(s/def :game/player (s/keys :req-un [:game-player/spirit :game-player/rating]
                            :opt-un [:game-player/aspect]))
(s/def :game/players (s/map-of string? :game/player))
(s/def :users/players (s/map-of string? map?))
(s/def :users/game (s/keys :req-un [:game/timestamp :game/outcome :game/num-turns :game/players]
                           :opt-un [:game/adversaries]))
(s/def :users/games (s/coll-of :users/game))
(s/def :users/spec (s/keys :req-un [:users/players :users/games]))

(def ^:private default-filename "resources/users.edn")

(defprotocol IUserService
  (store [this users])                                      ; Probably want this to be private/hidden
  (all-users [this])
  (player? [this name])
  (players? [this names])
  (all-games [this])
  (add-game [this win? num-turns adversaries players]))

(defrecord UserService [^String filename users]
  IUserService
  (store [_ u] (spit filename (-> u
                                  (str/replace "," ",\n")
                                  (str/replace "} " "}\n")
                                  (str/replace "\n :rating" " :rating"))))
  (all-users [_] (-> @users :players keys sort))
  (player? [this name] (players? this [name]))
  (players? [this names] (when (seq names)
                           (let [u (set (all-users this))]
                             (and (apply distinct? names)
                                  (every? u names)))))

  (all-games [_] (:games @users))
  (add-game [this win? num-turns adversaries players]
    (swap! users update :games conj {:timestamp   (Date.)
                                     :outcome     win?
                                     :num-turns   num-turns
                                     :adversaries adversaries
                                     :players     players})
    (store this @users)))

(defn valid? [u] (s/valid? :users/spec u))
(defn parse-users
  ([] (parse-users default-filename))
  ([filename] (let [u (edn/read-string (slurp filename))]
                (assert (valid? u) (str "Invalid users at file " filename))
                u)))

(defn create-service
  ([] (create-service default-filename))
  ([filename] (create-service filename (parse-users filename)))
  ([filename user-data] (->UserService filename (atom user-data))))

(defmethod ig/init-key :spirit-island/users [_ {:keys [filename]}]
  (parse-users filename))

(defmethod ig/init-key :spirit-island/user-svc [_ {:keys [filename]}]
  (create-service filename))
