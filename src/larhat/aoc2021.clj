(ns larhat.aoc2021
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]
   [clojure.set :as cl-set]))

(defn day-1-1 [data]
  (as-> (map parse-int data) i
    (map > (rest i) i)
    (filter identity i)
    (count i)
    ))
(defn run-day-1-1 []
  (day-1-1 (inp-lines 1)))

(defn day-1-2 [data]
  (as-> (map parse-int data) i
    (map + (nthrest i 2) (rest i) i)
    (map > (rest i) i)
    (filter identity i)
    (count i)))
(defn run-day-1-2 []
  (day-1-2 (inp-lines 1)))

(def commands
  {"forward" (fn [arg {:keys [h d]}]
               {:h (+ h arg) :d d})
   "down" (fn [arg {:keys [h d]}]
            {:h h :d (+ d arg)})
   "up" (fn [arg {:keys [h d]}]
          {:h h :d (- d arg)})})
(defn day-2-common [data cmd-set]
  (as-> (map words data) i
    (reduce (fn [acc [cmd arg]]
              ((cmd-set cmd) (parse-int arg) acc))
      {:h 0 :d 0 :a 0} i)
    (* (:h i) (:d i))))
(defn run-day-2-1 []
  (day-2-common (inp-lines 2) commands))

(def commands-2
  {"forward" (fn [arg {:keys [h d a]}]
               {:h (+ h arg) :a a :d (+ d (* a arg))})
   "down" (fn [arg {:keys [h d a]}]
            {:h h :d d :a (+ a arg)})
   "up" (fn [arg {:keys [h d a]}]
          {:h h :d d :a (- a arg)})
   })
(defn run-day-2-2 []
  (day-2-common (inp-lines 2) commands-2))

(defn day-3-1 [data]
  (as-> (apply mapv vector data) i
    (map frequencies i)
    (reduce (fn [{:keys [gamma epsilon]} digit]
              {
               :gamma (conj gamma (key (apply max-key val digit)))
               :epsilon (conj epsilon (key (apply min-key val digit)))
               })
      {:gamma [] :epsilon []}
      i)
    (*
      (parse-int (apply str (:gamma i)) 2)
      (parse-int (apply str (:epsilon i)) 2))))
(defn run-day-3-1 []
  (day-3-1 (inp-lines 3)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
