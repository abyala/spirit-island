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
                            :games   []}})

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


