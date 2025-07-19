(ns dev
  (:require [integrant.core :as ig]
            [spirit-island.cli :as cli]
            [spirit-island.game :as g]
            [spirit_island.metadata :as m]
            [spirit-island.users :as u]))

(def state (-> (slurp "resources/config.edn")
               ig/read-string
               (dissoc :spirit-island/cli)
               ig/init))

(def user-service (:spirit-island/user-svc state))
(def metadata-service (:spirit-island/metadata-svc state))