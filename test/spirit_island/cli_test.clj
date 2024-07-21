(ns spirit-island.cli-test
  (:require [clojure.test :refer :all]
            [spirit-island.cli :as cli]
            [spirit_island.core :as c]))

(def game1 {:timestamp   #inst "2024-05-12T00:30:00.000-00:00",
            :outcome     :win,
            :num-turns   7,
            :adversaries {:england 3},
            :players     {"Andrew" {:spirit :fangs, :rating 4}}})
(def game2 {:timestamp   #inst "2024-05-12T01:30:00.000-00:00",
            :outcome     :loss,
            :num-turns   8,
            :adversaries {:england 4},
            :players     {"Andrew" {:spirit :river, :rating 2}}})
(def game3 {:timestamp   #inst "2024-05-12T02:30:00.000-00:00",
            :outcome     :win,
            :num-turns   9,
            :adversaries {:france 2},
            :players     {"Andrew" {:spirit :fangs, :aspect :encircle, :rating 4}}})
(def test-state {:metadata {:spirits     {:river      {:name "River Surges in Sunlight", :difficulty :low}
                                          :fangs      {:name "Sharp Fangs Behind the Leaves", :difficulty :moderate
                                                       :aspects {:encircle {}}}
                                          :many-minds {:name "Many Minds Move as One", :difficulty :moderate}}
                            :boards      [:a :b :c :d :e :f],
                            :adversaries {:england {:name "England"}
                                          :france  {:name "France"}}}
                 :users    {:players {"Andrew" {}
                                      "Marc"   {}}
                            :games   [game1 game2 game3]}})

(deftest parse-stat-filters-test
  (are [input] (nil? (cli/parse-stat-filters test-state input))
               "stats blah"
               "stats england;blah"
               "stats blah;Andrew"
               "stats Andrew=blah"
               "stats Andrew;england;Marc=blah")
  (are [n input] [= n (count (cli/parse-stat-filters test-state input))]
                 0 "stats"
                 0 "stats "
                 0 "stats   "
                 1 "stats england"
                 1 "stats Andrew"
                 1 "stats Andrew=river"
                 2 "stats england;Andrew"
                 2 "stats Andrew=river;Marc=many-minds"
                 3 "stats Andrew=river;Marc=many-minds;england"))

(deftest rounded-percent-test
  (are [expected n] (= (c/rounded-percent n) expected)
                    100 1
                    50 (/ 1 2)
                    33 (/ 1 3)
                    67 (/ 2 3)
                    100 (/ 3 3))
  (are [expected n d] (= (c/rounded-percent n d) expected)
                      100 1 1
                      50 1 2
                      33 1 3
                      67 2 3
                      100 3 3))

(deftest filtered-games-test
  (are [expected input] (= (cli/filtered-games test-state input) expected)
                        [game1 game2 game3] "stats"
                        [game1 game2 game3] "stats Andrew"
                        [game1 game2] "stats england"
                        [game3] "stats france"
                        [game1 game3] "stats Andrew=fangs"
                        [game1] "stats Andrew=fangs;england"
                        [game1] "stats england;Andrew=fangs"))
