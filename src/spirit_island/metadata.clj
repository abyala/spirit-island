(ns spirit_island.metadata
  (:require [clojure.edn :as edn]
            [spirit_island.core :refer [in?]]
            [integrant.core :as ig]))

(defn parse-metadata
  ([] (parse-metadata "resources/metadata.edn"))
  ([filename] (edn/read-string (slurp filename))))

(defn spirit-names [metadata]
  (map :name (vals (:spirits metadata))))

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
  ({"low"       :low, "easy" :low,
    "moderate"  :moderate, "medium" :moderate
    "high"      :high, "hard" :high
    "very-high" :very-high, "vhigh" :very-high, "very-hard" :very-high, "vhard" :very-high} s))

(defn adversary? [metadata s]
  (contains? (set (adversary-names metadata)) (keyword s)))

(defn spirit?
  ([metadata spirit] (spirit? metadata spirit nil))
  ([metadata spirit aspect]
   (when-some [{:keys [aspects]} (get-in metadata [:spirits (keyword spirit)])]
     (or (nil? aspect) (contains? aspects (keyword aspect))))))

(defn board? [metadata s]
  (in? (:boards metadata) (keyword s)))