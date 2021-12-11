(ns larhat.prelude
  (:require
   [clojure.string :as str]
   [clojure.set :as cl-set]))

(defn input [n]
  (let [name (if (number? n)
               (str "inp" n)
               n)]
    (slurp (format "resources/larhat/%s.txt" name))))

(defn words [s]
  (str/split s #"\s"))

(defn comma-sequence [s]
  (-> s
    str/trim
    (str/split #"\,")))

(defn lines [s]
  (str/split-lines s))

(defn phrases [s]
  (str/split s #"\n\n"))

(defn inp-words [n]
  (-> (input n) words))

(defn inp-lines [n]
  (-> (input n) lines))

(defn inp-phrases [n]
  (-> (input n) phrases))

(defn parse-int
  ([n]  (Integer. (str n)))
  ([n x] (Integer/parseInt n x)))

(defn parse-bin-int [n] (parse-int n 2))

(defn parse-long [n]
  (Long. n))

(defn bound [mi ma x]
  (min ma (max mi x)))

(defn num-grid [s]
  (mapv #(mapv parse-int %) (lines s)))

(defn inp-num-grid [n]
  (num-grid (input n)))
