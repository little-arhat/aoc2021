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

(t day7
  (is (= 341558 (run-day-7-1)))
  (is (= 93214037 (run-day-7-2))))

(t day8
  (is (= 452 (run-day-8-1)))
  (is (= 1096964 (run-day-8-2))))

(t day9
  (is (= 585 (run-day-9-1)))
  (is (= 827904 (run-day-9-2))))

(t day10
  (is (= 387363 (run-day-10-1)))
  (is (= 4330777059 (run-day-10-2))))

(t day11
  (is (= 1743 (run-day-11-1)))
  (is (= 364 (run-day-11-2))))

(t day12
  (is (= 4792 (run-day-12-1)))
  (is (= 133360 (run-day-12-2))))

(t day13
  (is (= 664 (run-day-13-1)))
  (is (=
        "#### ####   ## #  # #### #    ###  #   \n#    #       # # #     # #    #  # #   \n###  ###     # ##     #  #    ###  #   \n#    #       # # #   #   #    #  # #   \n#    #    #  # # #  #    #    #  # #   \n#### #     ##  #  # #### #### ###  ####\n"
        (run-day-13-2))))

(t day14
  (is (= 2621 (run-day-14-1)))
  (is (= 2843834241366 (run-day-14-2))))

(t day15
  (is (= 487 (run-day-15-1)))
  (is (= 2821 (run-day-15-2))))

(t day16
  (is (= 843 (run-day-16-1)))
  (is (= 5390807940351 (run-day-16-2))))
