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
    (num-grid b)))


(defn mark [board number]
  (map-grid (fn [cell]
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

(def l-1478 #{2 3 4 7})
(defn is-1478 [wd]
  (-> wd count l-1478 some?))

(defn str-sort [s]
  (apply str (sort s)))

(defn day-8-1 [data]
  (let [c1 (str/replace data " |\n" " | ")
        codes (lines c1)
        xf (comp
             (map #(str/split % #" \| "))
             (map last)
             (map words)
             (map #(filter is-1478 %))
             (map count))]
    (transduce xf + codes)))
(defn run-day-8-1 []
  (day-8-1 (input 8)))

(defn seg-includes? [seg other-seg]
  (clojure.set/superset? (set seg) (set other-seg)))
(defn seg-intersection [seg other-seg]
  (clojure.set/intersection (set seg) (set other-seg)))

(defn decypher [[inputs results]]
  (let [sinputs (map str-sort inputs)
        sresults (map str-sort results)
        by-length (->> (concat sinputs sresults)
                    set
                    (group-by count))
        [d1] (by-length 2)
        [d7] (by-length 3)
        [d4] (by-length 4)
        [d8] (by-length 7)
        md-5 (by-length 5)
        [d3] (filter #(seg-includes? % d1) md-5)
        md-5' (remove #{d3} md-5)
        [d5] (filter #(= 3 (count (seg-intersection % d4))) md-5')
        [d2] (remove #{d5} md-5')
        md-6 (by-length 6)
        [d6] (remove #(seg-includes? % d1) md-6)
        md-6' (remove #{d6} md-6)
        [d9] (filter #(seg-includes? % d4) md-6')
        [d0] (remove #{d9} md-6')
        mp (clojure.set/map-invert (into {} (map-indexed vector)
                                     [d0 d1 d2 d3 d4 d5 d6 d7 d8 d9]))
        iresults (map mp sresults)
        sres (apply str iresults)]
    (parse-int sres)))

(defn day-8-2 [data]
  (let [codes (lines (str/replace data " |\n" " | "))
        xf (comp
             (map #(str/split % #" \| "))
             (map #(map words %))
             (map decypher))]
    (transduce xf + codes)))
(defn run-day-8-2 []
  (day-8-2 (input 8)))


(def axis4 #{[-1 0] [1 0] [0 1] [0 -1]})
(def axis8 #{[-1 0] [1 0] [0 1] [0 -1]
             [-1 -1] [-1 1] [1 -1] [1 1]})
(defn neighbours [indices point]
  (mapv #(mapv + point %) indices))
(def neighbours4 (partial neighbours axis4))
(def neighbours8 (partial neighbours axis8))

(defn adjacent4 [grid point]
  (->> (neighbours4 point)
    (map (grid-get grid))
    (remove nil?)))

(defn low-points [mp grid]
  (grid-select
    (fn [point el]
      (< el (apply min (adjacent4 grid point))))
    mp
    grid))

(defn day-9-1 [grid]
  (transduce
    (map inc)
    + (low-points last grid)))
(defn run-day-9-1 []
  (day-9-1 (inp-num-grid 9)))

(defn basin-at-low-point* [grid width height [x y]]
  (loop [to-scan #{[x y]}
         found #{}]
    (if-some [point (first to-scan)]
      (let [nbs (neighbours4 point)
            gg (grid-get grid)
            non-terminating (remove #(or (nil? (gg %)) (= 9 (gg %))) nbs)
            not-visited (remove found non-terminating)
            to-scan' (rest to-scan)
            to-scan'' (reduce conj to-scan' not-visited)
            found' (conj found point)]
        (recur to-scan'' found'))
      ; else
      found)))

(defn basin-at-low-point [grid point]
  (basin-at-low-point* grid (count (first grid)) (count grid) point))

(defn day-9-2 [grid]
  (->> (low-points first grid)
      (map #(basin-at-low-point grid %))
      (sort-by count)
      (reverse)
      (take 3)
      (map count)
      (reduce *)))
(defn run-day-9-2 []
  (day-9-2 (inp-num-grid 9)))

(def char-pairs {\< \>
                 \[ \]
                 \{ \}
                 \( \)})
(def open-char? (set (keys char-pairs)))
(def close-char? (set (vals char-pairs)))
(defn valid-pair? [op cl]
  (= (char-pairs op) cl))
(def invalid-score-table
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn invalid-line? [rr]
  (not= -1 (:pos rr)))

(defn score-invalid-line [rr]
  (-> rr :cl invalid-score-table))

(defn scan-line [l]
  (loop [remaining l
         opened (list )
         pos 0]
    (let [ch (first remaining)
          remaining' (rest remaining)]
      (cond
        (nil? ch) {:opened opened
                   :pos -1
                   :op nil
                   :cl nil} ;; no-error, maybe incomplete
        (open-char? ch) (recur remaining' (conj opened ch) (inc pos))
        (close-char? ch) (if (valid-pair? (first opened) ch)
                           (recur remaining' (rest opened) (inc pos))
                           {:opened opened
                            :pos    pos
                            :op     (first opened)
                            :cl     ch})))))

(defn day-10-1 [data]
  (let [xf
        (comp
          (map scan-line )
          (filter invalid-line?)
          (map score-invalid-line)
          )]
    (transduce xf + data)))
(defn run-day-10-1 []
  (day-10-1 (inp-lines 10)))

(defn autocomplete [rr]
  (->> rr
    :opened
    (map char-pairs)))

(def incomplete-score-table
  {\) 1
   \] 2
   \} 3
   \> 4})
(defn score-incomplete [extra-chars]
  (reduce
    (fn [acc char]
      (+ (incomplete-score-table char) (* 5 acc)))
    0
    extra-chars))

(defn middle [scores]
  (let [c (count scores)]
    (nth scores (/ c 2))))

(defn day-10-2 [data]
  (let [xf (comp
             (map scan-line)
             (remove invalid-line?)
             (map autocomplete)
             (map score-incomplete))]
    (->> data
      (sequence xf)
      sort
      middle)))
(defn run-day-10-2 []
  (day-10-2 (inp-lines 10)))

(defn ready-to-flash? [_p el]
  (and (not (nil? el)) (< 9 el)))

(defn octopi-step [pre-octopi]
  (let [octopi+ (map-grid inc pre-octopi)]
    (loop [octopi octopi+]
      (let [ready-to-flash (set (grid-select ready-to-flash? first octopi))
            nbs (mapcat neighbours8 ready-to-flash)
            to-inc (frequencies nbs)]
        (if (empty? ready-to-flash)
          octopi
                                        ; else
          (recur
            (map-grid-indexed
              (fn [p el]
                (cond
                  (nil? el)          el
                  (ready-to-flash p) nil
                  (to-inc p)         (+ el (to-inc p))
                  :else              el
                  ))
              octopi)))))))

(defn counting-octopi-step [{:keys [total-flashed
                                    step
                                    octopi]}]
  (let [octopi' (octopi-step octopi)
        c (atom 0)
        octopi'' (map-grid
                   (fn [el]
                     (if (nil? el)
                       (do
                         (swap! c inc)
                         0)
                       el))
                   octopi')]
    {:total-flashed (+ total-flashed @c)
     :step (inc step)
     :out-flashed-this-time @c
     :octopi octopi''}))

(defn day-11-1 [grid]
  (->> {:total-flashed 0 :step 0 :octopi grid}
    (iterate counting-octopi-step)
    (take 101)
    last
    :total-flashed))
(defn run-day-11-1 []
  (day-11-1 (inp-num-grid 11)))

(defn all-flashed? [{:keys [out-flashed-this-time octopi]}]
  (= out-flashed-this-time (reduce + (map count octopi))))

(defn day-11-2 [grid]
  (->> {:total-flashed 0 :step 0 :octopi grid}
    (iterate counting-octopi-step)
    (take-while (complement all-flashed?))
    last
    :step
    inc))
(defn run-day-11-2 []
  (day-11-2 (inp-num-grid 11)))

(defn cave-system [cave-defs]
  (->> cave-defs
    (map #(str/split % #"-"))
    (map #(map keyword %))
    (reduce
      (fn [acc [from to]]
        (-> acc
          (update from (fnil conj #{}) to)
          (update to (fnil conj #{}) from)))
      {})))

(defn big-cave? [cave]
  (not= (str cave) (str/lower-case cave)))

(defn cave-paths [rem-fn cs]
  (loop [to-scan [[:start (list ) {:start 0}]]
         paths #{}]
    (let [[current from seen-small-on-the-way] (first to-scan)
          to-scan' (rest to-scan)
          nbs (current cs)
          seen (if (big-cave? current)
                 seen-small-on-the-way
                 (update seen-small-on-the-way current (fnil inc 0)))
          nbs' (if (= current :end)
                 (list)
                 (remove (rem-fn seen) nbs))
          from' (cons current from)
          nbs'' (zip nbs' (repeat from') (repeat seen))
          unseen (concat nbs'' to-scan')
          paths' (if (= current :end)
                   (conj paths from')
                   paths)]
      (if (empty? unseen)
        (map reverse paths')
                                        ; else
        (recur
          unseen
          paths')))))

(defn day-12-1 [data]
  (->> data
    cave-system
    (cave-paths identity)
    count))
(defn run-day-12-1 []
  (day-12-1 (inp-lines 12)))

(defn allow-at-most-2-rem-fn [seen-this-path]
  (let [seen-twice? (some #(<= 2 (last %)) seen-this-path)]
    (fn [cave]
      (cond
        (= :start cave) true
        (nil? (seen-this-path cave)) false
        :else seen-twice?))))

(defn day-12-2 [data]
  (->> data
    cave-system
    (cave-paths allow-at-most-2-rem-fn)
    count))
(defn run-day-12-2 []
  (day-12-2 (inp-lines 12)))

(defn parse-fold [f]
  (let [pf (fn [[c i]] [(keyword c) (parse-int i)])]
    (-> f
      (str/replace "fold along " "")
      (str/split #"=")
      pf)))

(def axis-m {:x 0 :y 1})
(defn fold-one [coords [axis fold-point]]
  (update
    coords
    (axis axis-m)
    #(- (* 2 fold-point) %)))

(defn fold-sheet [sheet [axis fold-point]]
  (let [k (axis axis-m)
        rf #(= fold-point (get % k))
        pbf #(< fold-point (get % k))
        sf #(get % k)
        sh' (sort-by sf sheet)
        xf (comp
             (remove rf)
             (partition-by pbf))
        [stays folding] (sequence xf sh')
        folding' (map #(fold-one % [axis fold-point]) folding)]
    (set (concat stays folding'))))

(defn parse-coord [c]
  (->> c
    comma-sequence
    (mapv parse-int)))

(defn parse-paper [ls]
  (let [xf (comp
             (remove str/blank?)
             (partition-by #(str/includes? % "fold")))
        [coords folds] (sequence xf ls)]
    [(set (map parse-coord coords))
     (map parse-fold folds)]))

(defn day-13-1 [data]
  (let [[sheet folds] (parse-paper data)
        sheet' (fold-sheet sheet (first folds))]
    (count sheet')))
(defn run-day-13-1 []
  (day-13-1 (inp-lines 13)))

(defn paper-sheet [pp]
  (let [w (inc (apply max (map first pp)))
        h (inc (apply max (map last pp)))]
    (apply str
      (forv [y (range h)]
        (str
          (apply str (forv [x (range w)]
                       (if (pp [x y])
                         "#"
                         " ")))
          "\n")))))

(defn day-13-2 [data]
  (let [[sheet folds] (parse-paper data)]
    (paper-sheet (reduce fold-sheet sheet folds))))
(defn run-day-13-2 []
  (day-13-2 (inp-lines 13)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
