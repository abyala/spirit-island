(ns spirit-island.cli-test
  (:require [clojure.test :refer :all]
            [spirit-island.cli :as cli]))

(def test-state {:metadata {:spirits     {:river      {:name "River Surges in Sunlight", :difficulty :low}
                                          :fangs      {:name "Sharp Fangs Behind the Leaves", :difficulty :moderate}
                                          :many-minds {:name "Many Minds Move as One", :difficulty :moderate}}
                            :boards      [:a :b :c :d :e :f],
                            :adversaries {:england {:name "England"}
                                          :france  {:name "France"}}}
                 :users    {:players {"Andrew" {}
                                      "Marc"   {}}
                            :games   [{:timestamp   #inst "2024-05-12T00:30:00.000-00:00",
                                       :outcome     :win,
                                       :num-turns   7,
                                       :adversaries {:england 3},
                                       :players     {"Andrew" {:spirit :fangs, :rating 4}}}
                                      {:timestamp   #inst "2024-05-12T01:30:00.000-00:00",
                                       :outcome     :loss,
                                       :num-turns   8,
                                       :adversaries {:england 4},
                                       :players     {"Andrew" {:spirit :river, :rating 2}}}
                                      {:timestamp   #inst "2024-05-12T02:30:00.000-00:00",
                                       :outcome     :win,
                                       :num-turns   9,
                                       :adversaries {:france 2},
                                       :players     {"Andrew" {:spirit :fangs, :rating 4}}}]}})

(deftest parse-record-game-test
  (testing "Invalid inputs"
    (are [s] (nil? (cli/parse-record-game test-state s))
             "record-game"
             "record-game asdfasdf"
             "record-game winner 7 england=4 Andrew=river,b,4"
             "record-game win 100 england=4 Andrew=river,b,4"
             "record-game win 7 englanddasd=4 Andrew=river,b,4"
             "record-game win 7 england=44 Andrew=river,b,4"
             "record-game win 7 england,4 Andrew=river,b,4"
             "record-game win 7 england=4 Andrew=river,4"
             "record-game win 7 england=4 Andrewwwww=river,b,4"
             "record-game win 7 england=4 Andrew=riverrrr,b,4"
             "record-game win 7 england=4 Andrew=river,b,47"
             "record-game win 7 england=4 Andrew,river,b,4"
             "record-game win 7 england=4 Andrew=river,b,4;Marc=fangs,4"
             "record-game win 7 england=4 Andrew=river,b;Marc=fangs,c,5"))
  (are [expected s] (= expected (cli/parse-record-game test-state s))
                    {:win?    :win, :turns 7, :adversaries {:england 4},
                     :players {"Andrew" {:spirit :river, :board :b,
                                         :rating 4}}}
                    "record-game win 7 england=4 Andrew=river,b,4"

                    {:win?    :win, :turns 7, :adversaries {:england 4},
                     :players {"Andrew" {:spirit :river, :board :b, :rating 4}
                               "Marc"   {:spirit :fangs, :board :c, :rating 3}}}
                    "record-game win 7 england=4 Andrew=river,b,4;Marc=fangs,c,3"

                    {:win?    :win, :turns 7, :adversaries {:england 4, :france 2},
                     :players {"Andrew" {:spirit :river, :board :b, :rating 4}}}
                    "record-game win 7 england=4;france=2 Andrew=river,b,4"

                    {:win?    :win, :turns 4, :adversaries {},
                     :players {"Andrew" {:spirit :many-minds, :board :d, :rating 4}}}
                    "record-game win 4 none Andrew=many-minds,d,4"))

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

(deftest filtered-stats-test
  (are [expected input] (= expected (cli/filtered-stats test-state input))
                        {:num 3 :win-rate 67} "stats"
                        {:num 3 :win-rate 67} "stats Andrew"
                        {:num 2 :win-rate 50} "stats england"
                        {:num 2 :win-rate 100} "stats Andrew=fangs"
                        {:num 2 :win-rate 50} "stats england;Andrew"
                        {:num 1 :win-rate 100} "stats england;Andrew=fangs"))
