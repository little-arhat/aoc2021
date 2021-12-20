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
  ([n]  (Long. (str n)))
  ([n x] (Long/parseLong n x)))

(defn parse-bin-int [n] (parse-int n 2))

(defn parse-long [n]
  (Long. n))

(defn bound [mi ma x]
  (min ma (max mi x)))

(defn num-grid [s]
  (if (seq? s)
    (mapv #(mapv parse-int %) s)
    (mapv #(mapv parse-int %) (lines s))))

(defn inp-num-grid [n]
  (num-grid (input n)))

(defn map-grid [f grid]
  (mapv #(mapv f %) grid))

(defn grid-get
  ([grid]
   (fn [[x y]]
     (get-in grid [y x])))
  ([grid [x y]]
   (get-in grid [y x])))

(defn grid-select [pred mp grid]
  (apply concat
    (keep-indexed
      (fn [y row]
        (not-empty (keep-indexed
                     (fn [x el]
                       (when (pred [x y] el)
                         (mp [[x y] el])))
                     row)))
      grid)))

; https://github.com/cloojure/tupelo/blob/c37b8aeb382697127b825a9f75a3e9c8e99290ed/src/cljc/tupelo/core.cljc#L667
(defmacro forv
  "Like clojure.core/for but returns results in a vector.
  Wraps the loop body in a `do` as with `doseq`. Not lazy."
  [& forms]
  (let [bindings-vec (first forms)
        body-forms   (rest forms)]
    `(vec (for ~bindings-vec
            (do ~@body-forms)))))

(defn map-grid-indexed [f grid]
  (forv [y (range (count grid))]
    (forv [x (range (count (first grid)))]
      (f [x y] (grid-get grid [x y])))))

(defn concat* [xs]
  (apply concat xs))

(defn comma-ints [s]
  (as-> (str/trim s) x
    (str/split x #"\,")
    (mapv parse-int x)))
