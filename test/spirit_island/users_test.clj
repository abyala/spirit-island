(ns spirit-island.users-test
  (:require [clojure.test :refer :all]
            [spirit-island.users :as u]))

(def test-users {:players (zipmap ["Andrew" "Scotty" "Marc"] (repeat {}))})

(deftest player?-test
  (let [service (u/create-service nil test-users)]
    (testing "Collection of users"
      (are [names] (u/players? service names)
                   ["Andrew"]
                   ["Andrew" "Scotty"]
                   ["Marc" "Scotty"])
      (are [names] (not (u/players? service names))
                   []
                   ["Bob"]
                   ["Andrew" "Bob"]
                   ["Bob" "Scotty"]))

    (testing "Single user"
      (are [name] (u/player? service name)
                  "Andrew"
                  "Scotty")
      (are [name] (not (u/player? service name))
                  nil
                  "Bob"))))