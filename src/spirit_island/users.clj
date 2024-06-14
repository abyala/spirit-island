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
(s/def :game/adversaries (s/map-of keyword? pos-int?))
(s/def :game/player (s/keys :req-un [:game-player/spirit :game-player/rating]
                            :opt-un [:game-player/aspect]))
(s/def :game/players (s/map-of string? :game/player))
(s/def :users/players (s/map-of string? map?))
(s/def :users/game (s/keys :req-un [:game/timestamp :game/outcome :game/num-turns :game/players]
                           :opt-un [:game/adversaries]))
(s/def :users/games (s/coll-of :users/game))
(s/def :users/spec (s/keys :req-un [:users/players :users/games]))


(def ^:private default-filename "resources/users.edn")

(defn valid? [users] (s/valid? :users/spec users))
(defn parse-users
  ([] (parse-users default-filename))
  ([filename] (let [u (edn/read-string (slurp filename))]
                (assert (valid? u) (str "Invalid users at file " filename))
                u)))

(defn store-users [users]
  (spit default-filename (-> users
                             (str/replace "," ",\n")
                             (str/replace "} " "}\n")
                             (str/replace "\n :rating" " :rating"))))

(defn all-users [users]
  (-> users :players keys sort))

(defn player? [users names]
  (if (string? names) (recur users [names])
                      (when (seq names)
                        (let [u (set (all-users users))]
                          (and (apply distinct? names)
                               (every? u names))))))

(defn all-games [users]
  (-> users :games))

(defmethod ig/init-key :spirit-island/users [_ {:keys [filename]}]
  (parse-users filename))

(defn add-game [users win? num-turns adversaries players]
  (let [u (update users :games conj {:timestamp   (Date.)
                                     :outcome     win?
                                     :num-turns   num-turns
                                     :adversaries adversaries
                                     :players     players})]
    (store-users u)
    u))
