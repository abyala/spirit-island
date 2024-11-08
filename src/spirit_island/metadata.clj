(ns spirit_island.metadata
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [spirit_island.core :refer [in? map-from-aliases]]
            [integrant.core :as ig]))

(s/def :shared/name string?)
(s/def :spirit/difficulty #{:low :moderate :high :very-high})
(s/def :spirit/aspect (s/keys :req-un [:shared/name]))
(s/def :spirit/aspects (s/map-of keyword :spirit/aspect))
(s/def :metadata/spirit (s/keys :req-un [:shared/name :spirit/difficulty]
                                :opt-un [:spirit/aspects]))
(s/def :metadata/spirits (s/map-of keyword? :metadata/spirit))
(s/def :metadata/boards (s/coll-of keyword?))
(s/def :metadata/adversary (s/keys :req-un [:shared/name]))
(s/def :metadata/adversaries (s/map-of keyword? :metadata/adversary))
(s/def :metadata/metadata (s/keys :req-un [:metadata/spirits :metadata/boards :metadata/adversaries]))

(def ^:private default-filename "resources/metadata.edn")

(defn valid? [metadata] (s/valid? :metadata/metadata metadata))
(defn parse-metadata
  ([] (parse-metadata default-filename))
  ([filename] (let [m (edn/read-string (slurp filename))]
                (assert (valid? m) (str "Invalid metadata at file " filename))
                m)))

(defn difficulty-for-string [s]
  (get (map-from-aliases {:low       ["low" "easy"]
                          :moderate  ["moderate" "medium"]
                          :high      ["high" "hard"]
                          :very-high ["very-high" "veryhigh" "vhigh" "very-hard" "veryhard" "vhard"]}) s))

(defprotocol IMetadataService
  (spirit-ids [svc] "Returns the IDs for all known spirits")
  (spirit-names [svc] "Returns the names for all known spirits")
  (spirits-and-aspects [svc])
  (adversary? [svc s])
  (adversary-names [svc])
  (all-boards [svc])
  (by-id [svc name])
  (spirits-of-difficulty [svc difficulty])
  (board? [svc s])
  (spirit?
    [svc spirit]
    [svc spirit aspect]))

(defrecord MetadataService [filename metadata]
  IMetadataService
  (spirit-ids [_] (-> metadata :spirits keys))

  (spirit-names [_] (map :name (vals (:spirits metadata))))

  (spirits-and-aspects [_]
    (letfn [(extract [{:keys [name aspects]}] (cond-> {:name name}
                                                      (seq aspects) (assoc :aspects (map :name (vals aspects)))))]
      (map extract (vals (:spirits metadata)))))

  (adversary-names [_] (-> metadata :adversaries keys sort))

  (adversary? [this s]
    (contains? (set (adversary-names this)) (keyword s)))

  (all-boards [_] (:boards metadata))

  (spirits-of-difficulty [_ difficulty]
    {:pre [(#{:low :moderate :high :very-high} difficulty)]}
    (sort (keep (fn [[name {d :difficulty}]] (when (= d difficulty) name))
                (:spirits metadata))))

  (by-id [_ id] (get-in metadata [:spirits id]))
  (board? [_ s]
    (in? (:boards metadata) (keyword s)))
  (spirit? [this spirit] (spirit? this spirit nil))
  (spirit? [_ spirit aspect]
    (when-some [{:keys [aspects]} (get-in metadata [:spirits (keyword spirit)])]
      (or (nil? aspect) (contains? aspects (keyword aspect))))))

(defn create-service
  ([] (create-service default-filename))
  ([filename] (create-service filename (parse-metadata filename)))
  ([filename metadata] (->MetadataService filename metadata)))

(defmethod ig/init-key :spirit-island/metadata-svc [_ {:keys [filename]}]
  (create-service filename))
