(ns spirit_island.core-test
  (:require [clojure.test :refer :all]
            [spirit_island.core :as c]))

(deftest only-when-test
  (testing "Fails the test function"
    (are [f v] (nil? (c/only-when f v))
               even? 1
               seq []
               (partial every? even?) [2 3 4]))
  (testing "Passes the test function"
    (are [f v] (= v (c/only-when f v))
               even? 2
               seq [:a :b :c]
               (partial every? even?) [2 4 6])))

(deftest numeric?-test
  (testing "Not numbers"
    (are [v] (false? (c/numeric? v))
             nil
             ""
             "abc"
             "one"))
  (testing "Are numbers"
    (are [v] (true? (c/numeric? v))
             "2"
             "0"
             "567"
             "-1")))

(deftest parse-long-within-range-test
  (are [expected s low high] (= expected (c/parse-long-within-range s low high))
                             5 "5" 5 5
                             5 "5" 0 5
                             5 "5" 5 8
                             0 "0" -100 100
                             -5 "-5" -10 0)
  (are [s low high] (nil? (c/parse-long-within-range s low high))
                    nil 0 10
                    "" 0 10
                    "5" 6 10
                    "5" 0 4
                    "5a" 0 10
                    "-5" 0 10))

(deftest in?-test
  (are [coll v] (true? (c/in? coll v))
                ["a"] "a"
                [:a :b] :a
                [:a :b] :b
                [:a :a :b :b] :b)
  (are [coll v] (nil? (c/in? coll v))
                nil true
                [] true
                ["a"] "A"
                [:a :b] :A
                [:a :a :b :b] :c))