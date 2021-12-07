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

(defn transpose [data]
  (apply mapv vector data))
(defn day-3-1 [data]
  (as-> (transpose data) i
    (map frequencies i)
    (reduce (fn [{:keys [gamma epsilon]} digit]
              {
               :gamma (conj gamma (key (apply max-key val digit)))
               :epsilon (conj epsilon (key (apply min-key val digit)))
               })
      {:gamma [] :epsilon []}
      i)
    (*
      (parse-bin-int (apply str (:gamma i)))
      (parse-bin-int (apply str (:epsilon i))))))
(defn run-day-3-1 []
  (day-3-1 (inp-lines 3)))

(defn oxygen-selector [zero one]
  (if (= (count zero) (count one))
    one
    (max-key count zero one)))
(defn co2-selector [zero one]
  (if (= (count zero) (count one))
    zero
    (min-key count zero one)))
(defn find-sensors [numbers selector index]
  (let [split-by-bit (group-by #(get % index) numbers)
        zero-bit (split-by-bit \0)
        one-bit (split-by-bit \1)
        selected (selector zero-bit one-bit)]
    (if (= 1 (count selected))
      (first selected)
                                        ; else
      (recur selected selector (inc index)))))
(defn day-3-2 [data]
  (let [oxygen (find-sensors data oxygen-selector 0)
        co2 (find-sensors data co2-selector 0)]
    (* (parse-bin-int oxygen) (parse-bin-int co2))))
(defn run-day-3-2 []
  (day-3-2 (inp-lines 3)))

(defn to-board [long-line]
  (as-> (lines long-line) b
    (map #(remove str/blank? (words %)) b)
    (mapv #(mapv parse-int %) b)))

(defn map-board [f board]
  (mapv #(mapv f %) board))
(defn mark [board number]
  (map-board (fn [cell]
               (if (= number cell) nil cell)) board))

(defn only-nils? [xs]
  (every? nil? xs))

(defn winning-board [board]
  (if (or
        (some only-nils? board)
        (some only-nils? (transpose board)))
    board
    nil))

(defn safe-plus [a b] (+ (or a 0) (or b 0)))
(defn score-board [board]
  (reduce safe-plus (map #(reduce safe-plus %) board)))

(defn day-4-1 [numbers boards]
  (let [call (first numbers)
        boards' (mapv #(mark % call) boards)
        winner (some winning-board boards')]
    (if (some? winner)
      (* call (score-board winner))
                                        ; else
      (recur (rest numbers) boards'))))

(defn run-day-4-helper [f i]
  (f
    (->> i first comma-sequence (map parse-int))
    (->> i rest  (map to-board))))
(defn run-day-4-1 []
  (run-day-4-helper day-4-1 (inp-phrases 4)))

(defn day-4-2
  ([numbers boards]
   (day-4-2 numbers boards nil nil))
  ([numbers boards last-winner last-call]
   (let [call (first numbers)
         left (rest numbers)
         boards' (mapv #(mark % call) boards)
         winners (filter winning-board boards')
         winner (first winners)
         boards'' (remove (set winners) boards')]
     (if (empty? left)
       (if (some? winner)
         (* call (score-board winner))
         (* last-call (score-board last-winner)))
       (recur left
         boards''
         (or winner last-winner)
         (if (some? winner) call last-call))))))
(defn run-day-4-2 []
  (run-day-4-helper day-4-2 (inp-phrases 4)))

(defn mapv-split [f pat inp]
  (mapv f (str/split inp pat)))

(defn arrows [ls]
  (map
    (fn [l]
      (mapv-split
        (fn [p]
          (mapv-split parse-int #"," p))
        #" -> "
        l))
    ls))

(defn vert-or-hor [[[x1 y1] [x2 y2]]]
  (or
    (= x1 x2)
    (= y1 y2)))

(defn segments [a b]
  (if (< a b)
    (range a (inc b))
    (range a (dec b) -1)))

(defn zip [& xs]
  (apply map vector xs))

(defn mark-line [sea-bed coords]
  (reduce (fn [sb [x y]]
            (update-in sb [[x y]] (fnil inc 0)))
    sea-bed coords))

(defn mark-sea-bed-hv [sea-bed [[x1 y1] [x2 y2]]]
  (if (= x1 x2)
    (mark-line sea-bed
      (zip (repeat x1) (segments y1 y2)))
    (mark-line sea-bed
      (zip (segments x1 x2) (repeat y1)))))

(defn day-5-1 [data]
  (as-> (arrows data) i
    (filter vert-or-hor i)
    (reduce mark-sea-bed-hv
      {}
      i)
    (vals i)
    (filter #(< 1 %) i)
    (count i)))
(defn run-day-5-1 []
  (day-5-1 (inp-lines 5)))

(defn mark-sea-bed [sea-bed [[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2)
    (mark-line sea-bed
      (zip (repeat x1) (segments y1 y2)))
    (= y1 y2)
    (mark-line sea-bed
      (zip (segments x1 x2) (repeat y1)))
                                        ; else: diag
    :else
    (mark-line sea-bed
      (zip (segments x1 x2) (segments y1 y2)))))

(defn day-5-2 [data]
  (as-> (arrows data) i
    (reduce mark-sea-bed
      {}
      i)
    (vals i)
    (filter #(< 1 %) i)
    (count i)))
(defn run-day-5-2 []
  (day-5-2 (inp-lines 5)))

(defn plus-n [n]
  (fnil #(+ n %) 0))

(defn one-day-of-fish [fish]
  (reduce (fn [acc [fish-timer fish-count]]
            (if (zero? fish-timer)
              (-> acc
                (update-in [8] (plus-n fish-count))
                (update-in [6] (plus-n fish-count)))
              ; else
              (update-in acc [(dec fish-timer)] (plus-n fish-count))))
    {}
    fish))

(defn day-6-1 [days-of-life data]
  (->> (map parse-int data)
    frequencies
    (iterate one-day-of-fish)
    (take (inc days-of-life))
    last
    vals
    (reduce +)))
(defn run-day-6-1 []
  (day-6-1 80 (comma-sequence (input 6))))
(defn run-day-6-2 []
  (day-6-1 256 (comma-sequence (input 6))))

;; rosetta
(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn day-7-1 [data]
  (let [poss (map parse-int data)
        md (median poss)
        costs (map #(Math/abs (- % md)) poss)]
    (reduce + costs)))
(defn run-day-7-1 []
  (day-7-1 (comma-sequence (input 7))))

(defn day-7-2 [data]
  (let [poss (map parse-int data)
        sm (reduce + poss)
        cnt (count poss)
        avg (-> (/ sm cnt) int)
        costs (map #(->> (- % avg) Math/abs inc range (reduce +)) poss)]
    (reduce + costs)))
(defn run-day-7-2 []
  (day-7-2 (comma-sequence (input 7))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
