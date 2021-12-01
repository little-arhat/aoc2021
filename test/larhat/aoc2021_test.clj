(ns larhat.aoc2020-test
   (:require [clojure.test :refer :all]
             [larhat.aoc2020 :refer :all]))

(defmacro t [n & body]
  `(deftest ~n
     (testing ~n ~@body)))
                                        ; regression
(t day1
  (is (= 1233 (run-day-1-1)))
  (is (= 1275 (run-day-1-1))))
