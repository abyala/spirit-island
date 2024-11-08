(ns metadata_test
  (:require [clojure.test :refer :all]
            [spirit_island.metadata :as m]))

(def test-metadata {:spirits     {:river      {:name "River Surges in Sunlight", :difficulty :low}
                                  :fangs      {:name    "Sharp Fangs Behind the Leaves", :difficulty :moderate
                                               :aspects {:encircle {}}}
                                  :many-minds {:name "Many Minds Move as One", :difficulty :moderate}}
                    :boards      [:a :b :c :d :e :f],
                    :adversaries {:england {:name "England"}
                                  :france  {:name "France"}}})

(deftest spirit?-test
  (let [service (m/create-service "" test-metadata)]
    (testing "No aspects"
      (are [spirit] (not (m/spirit? service spirit))
                    nil
                    ""
                    "River Surges in Sunlight"
                    "rivers")
      (are [spirit] (m/spirit? service spirit)
                    "river"
                    "fangs"
                    "many-minds"))
    (testing "With aspects"
      (are [spirit aspect] (not (m/spirit? service spirit aspect))
                           "river" "encircle"
                           "fangs" "mew")
      (are [spirit aspect] (m/spirit? service spirit aspect)
                           "fangs" "encircle"))))
