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

(t day4
  (is (= 10374 (run-day-4-1)))
  (is (= 24742 (run-day-4-2))))

(t day5
  (is (= 6113 (run-day-5-1)))
  (is (= 20373 (run-day-5-2))))

(t day6
  (is (= 380243 (run-day-6-1)))
  (is (= 1708791884591 (run-day-6-2))))
