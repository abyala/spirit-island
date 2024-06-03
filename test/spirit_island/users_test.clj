(ns spirit-island.users-test
  (:require [clojure.test :refer :all]
            [spirit-island.users :as u]))

(def test-users {:players (zipmap ["Andrew" "Scotty" "Marc"] (repeat {}))})

(deftest valid?-test
  (testing "Collection of users"
    (are [names] (u/player? test-users names)
                 ["Andrew"]
                 ["Andrew" "Scotty"]
                 ["Marc" "Scotty"])
    (are [names] (not (u/player? test-users names))
                 []
                 ["Bob"]
                 ["Andrew" "Bob"]
                 ["Bob" "Scotty"]))

  (testing "Single user"
    (are [name] (u/player? test-users name)
                "Andrew"
                "Scotty")
    (are [name] (not (u/player? test-users name))
                nil
                "Bob")))