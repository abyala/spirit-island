(ns cli_parse_test
  (:require [clojure.test :refer :all]
            [spirit-island.cli-parse :as clip]))


(deftest parse-record-game-test
  (testing "Invalid inputs"
    (are [error s] (= (clip/parse-record-game s) error)
                   :invalid-win? "record-game"
                   :invalid-win? "record-game asdfasdf"
                   :invalid-win? "record-game winner 7 england=4 Andrew=river,b,4"
                   :invalid-turns "record-game win 100 england=4 Andrew=river,b,4"
                   :invalid-adversary-level "record-game win 7 england=44 Andrew=river,b,4"
                   :invalid-adversary "record-game win 7 england,4 Andrew=river,b,4"
                   :invalid-player "record-game win 7 england=4 Andrew=river,4"
                   :invalid-player-rating "record-game win 7 england=4 Andrew=river,b,47"
                   :invalid-spirit "record-game win 7 england=4 Andrew,river,b,4"
                   :invalid-player "record-game win 7 england=4 Andrew=river,b,4;Marc=fangs,4"
                   :invalid-player "record-game win 7 england=4 Andrew=river,b;Marc=fangs,c,5"))
  (are [expected s] (= expected (clip/parse-record-game s))
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
                    "record-game win 4 none Andrew=many-minds,d,4"

                    {:win?    :win, :turns 4, :adversaries {},
                     :players {"Andrew" {:spirit :fangs, :aspect :encircle, :board :d, :rating 4}}}
                    "record-game win 4 none Andrew=fangs,encircle,d,4"))