(ns spirit_island.metadata
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [spirit_island.core :refer [in? map-from-aliases no-nil-vals]]
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

(defn valid? [metadata] (s/valid? :metadata/metadata metadata))
(defn parse-metadata
  ([] (parse-metadata "resources/metadata.edn"))
  ([filename] (let [m (edn/read-string (slurp filename))]
                (assert (valid? m) (str "Invalid metadata at file " filename))
                m)))

(defn spirit-names [metadata]
  (map :name (vals (:spirits metadata))))

(defn spirits-and-aspects [metadata]
  (map #(no-nil-vals (hash-map :name (:name %) :aspects (seq (map :name (vals (:aspects %))))))
       (vals (:spirits metadata))))

(defn adversary-names [metadata]
  (-> metadata :adversaries keys sort))

(defn all-boards [metadata]
  (:boards metadata))

(defn spirits-of-difficulty [metadata difficulty]
  {:pre [(#{:low :moderate :high :very-high} difficulty)]}
  (sort (keep (fn [[name {d :difficulty}]] (when (= d difficulty) name))
              (:spirits metadata))))

(defn by-id [metadata name]
  (get-in metadata [:spirits name]))

(defmethod ig/init-key :spirit-island/metadata [_ {:keys [filename]}]
  (parse-metadata filename))

(defn difficulty-for-string [s]
  (get (map-from-aliases {:low       ["low" "easy"]
                          :moderate  ["moderate" "medium"]
                          :high      ["high" "hard"]
                          :very-high ["very-high" "veryhigh" "vhigh" "very-hard" "veryhard" "vhard"]}) s))

(defn adversary? [metadata s]
  (contains? (set (adversary-names metadata)) (keyword s)))

(defn spirit?
  ([metadata spirit] (spirit? metadata spirit nil))
  ([metadata spirit aspect]
   (when-some [{:keys [aspects]} (get-in metadata [:spirits (keyword spirit)])]
     (or (nil? aspect) (contains? aspects (keyword aspect))))))

(defn board? [metadata s]
  (in? (:boards metadata) (keyword s)))