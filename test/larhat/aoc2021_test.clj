(ns larhat.aoc2021-test
   (:require [clojure.test :refer :all]
             [larhat.aoc2021 :refer :all]))

(defmacro t [n & body]
  `(deftest ~n
     (testing ~n ~@body)))
                                        ; regression
(t day1
  (is (= 1233 (run-day-1-1)))
  (is (= 1275 (run-day-1-2))))

(t day2
  (is (= 1882980 (run-day-2-1)))
  (is (= 1971232560 (run-day-2-2))))

(t day3
  (is (= 4006064 (run-day-3-1)))
  (is (= 5941884 (run-day-3-2))))
