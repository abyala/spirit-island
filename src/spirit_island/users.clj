(ns spirit-island.users
  (:require [clojure.edn :as edn]
            [clj-time.format :as f]
            [clojure.string :as str]
            [integrant.core :as ig]))

(def ^:private default-filename "resources/users.edn")

(defn parse-users
  ([] (parse-users default-filename))
  ([filename] (edn/read-string (slurp filename))))

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
  (let [u (update users :games conj {:timestamp   (f/unparse (f/formatters :date-time) nil)
                                     :outcome     win?
                                     :num-turns   num-turns
                                     :adversaries adversaries
                                     :players     players})]
    (store-users u)
    u))
