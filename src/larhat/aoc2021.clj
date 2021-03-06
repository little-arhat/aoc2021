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
    (->> i first comma-ints)
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
  (->> (comma-ints data)
    frequencies
    (iterate one-day-of-fish)
    (take (inc days-of-life))
    last
    vals
    (reduce +)))
(defn run-day-6-1 []
  (day-6-1 80 (input 6)))
(defn run-day-6-2 []
  (day-6-1 256 (input 6)))

;; rosetta
(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn day-7-1 [data]
  (let [poss (comma-ints data)
        md (median poss)
        costs (map #(Math/abs (- % md)) poss)]
    (reduce + costs)))
(defn run-day-7-1 []
  (day-7-1  (input 7)))

(defn day-7-2 [data]
  (let [poss (comma-ints data)
        sm (reduce + poss)
        cnt (count poss)
        avg (-> (/ sm cnt) int)
        costs (map #(->> (- % avg) Math/abs inc range (reduce +)) poss)]
    (reduce + costs)))
(defn run-day-7-2 []
  (day-7-2 (input 7)))

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
    (map #(vector % (grid-get grid %)))
    (remove #(nil? (last %)))))

(defn low-points [mp grid]
  (grid-select
    (fn [point el]
      (->> (adjacent4 grid point)
        (map last)
        (apply min)
        (< el)))
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
  (and el (< 9 el)))

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

(defn parse-paper [ls]
  (let [xf (comp
             (remove str/blank?)
             (partition-by #(str/includes? % "fold")))
        [coords folds] (sequence xf ls)]
    [(set (map comma-ints coords))
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

(defn parse-insertions [ris]
  (into {} (map #(str/split % #" -> ")  ris)))

(defn f-pairs [s]
  (map #(apply str %) (zip s (rest s))))

(defn pairs [s]
  (zip s (rest s)))

(defn pair-insert [insertions [pair count]]
  (let [replacement (insertions pair)
        [fl ll] pair]
    {(str fl replacement) count
     (str replacement ll) count}))

(defn mk-polymer-fn [insertions]
  (fn [fs]
    (apply merge-with +
      (map #(pair-insert insertions %) fs))))

(defn parse-polymers [data]
  (let [[[formula] _ raw-dict] (partition-by str/blank? data)
        dict (parse-insertions raw-dict)]
    [formula dict]))

(defn poly-score [freqs]
  (let [[_ mic] (apply min-key val freqs)
        [_ mac] (apply max-key val freqs)]
    (- mac mic)))

(defn poly-inc-last-char [ll freqs]
  (update freqs ll (fnil inc 0)))

(defn day-14-1 [data steps]
  (let [[formula insertions] (parse-polymers data)
        f (mk-polymer-fn insertions)
        ll (last formula)]
    (->> (frequencies (f-pairs formula))
      (iterate f)
      (drop steps)
      first
      (map (fn [[[fl _] c]] {fl c}))
      (apply merge-with +)
      (poly-inc-last-char ll)
      poly-score)))

(defn run-day-14-1 []
  (day-14-1 (inp-lines 14) 10))

(defn run-day-14-2 []
  (day-14-1 (inp-lines 14) 40))

(defn indices [grid]
  (concat*
    (for [y (range (count grid))]
      (for [x (range (count (first grid)))]
        [x y]))))

(defn cave-graph [grid]
  (reduce
    (fn [adj-map [x y]]
      (let [nbs (adjacent4 grid [x y])
            src-risk (grid-get grid [x y])]
        (reduce
          (fn [am' [[nx ny] n-risk :as nb]]
            (-> am'
              (update [x y] (fnil conj #{}) nb)
              (update [nx ny] (fnil conj #{}) [[x y] src-risk])))
          adj-map
          nbs)))
    {}
    (indices grid)))

;; https://stackoverflow.com/questions/671706/how-do-i-make-a-java-class-immutable-in-clojure
(defn make-pqueue [init init-prio]
  (sorted-set [init-prio init]))

(defn pqueue-add [pq x prio]
  (conj pq [prio x]))

(defn pqueue-peek [pq]
  (first pq))

(defn pqueue-pop [pq]
  (let [top (first pq)]
    (disj pq top)))

(defn reduce-fn-d [{:keys [to-visit
                           distances
                           visited
                           current-risk
                           current-node] :as state}
                   [n-node n-risk]]
  (let [alt (+ current-risk n-risk)
        to-n-node (get distances n-node ##Inf)]
    (if (< alt to-n-node)
      {:to-visit (if (visited n-node)
                   to-visit
                   (pqueue-add to-visit n-node alt))
       :distances (assoc distances n-node alt)
       :visited visited
       :current-risk current-risk
       :current-node current-node}
                                        ; else
      state)))

(defn dijkstra [adj-map source]
  (loop [to-visit (make-pqueue source 0)
         visited #{}
         distances {source 0}]
    (if (empty? to-visit)
      distances
      (let [[current-risk node] (pqueue-peek to-visit)
            to-visit' (pqueue-pop to-visit)
            visited' (conj visited node)
            adjacent (get adj-map node #{})
            rr (reduce reduce-fn-d
                 {:to-visit to-visit'
                  :distances distances
                  :visited visited'
                  :current-risk current-risk
                  :current-node node}
                 adjacent)]
        (recur (rr :to-visit)
          visited'
          (rr :distances))))))

(defn day-15-1 [grid]
  (-> (cave-graph grid)
    (dijkstra [0 0])
    (get [(dec (count (first grid)))
          (dec (count grid))])))
(defn run-day-15-1 []
  (day-15-1 (inp-num-grid 15)))

(defn inc9 [n]
  (if (< n 9) (inc n) 1))
(defn ninc9 [t] #(nth (iterate inc9 %) t))
(defn enlarge [thing times mk-f]
  (apply concat
    thing
    (for [i (range 1 times)]
      (let [inc-f (ninc9 i)
            ff (mk-f inc-f)]
        (mapv ff thing)))))
(defn enlarge-vertically [grid times]
  (enlarge grid times (fn [inc-f] #(mapv inc-f %))))
(defn enlarge-horizontally [grid times]
  (mapv (fn [row]
          (vec (enlarge row times identity)))
    grid))

(defn day-15-2 [grid]
  (-> grid
    (enlarge-vertically 5)
    (enlarge-horizontally 5)
    cave-graph
    (dijkstra [0 0])
    (get [(dec (* 5 (count (first grid))))
          (dec (* 5 (count grid)))])))
(defn run-day-15-2 []
  (day-15-2 (inp-num-grid 15)))

(defn hex-bits [n]
  (nth ["0000"
        "0001"
        "0010"
        "0011"
        "0100"
        "0101"
        "0110"
        "0111"
        "1000"
        "1001"
        "1010"
        "1011"
        "1100"
        "1101"
        "1110"
        "1111"
        ] (Integer/parseInt (str n) 16)))

(defn hex-bits-s [ns]
  (apply str (map hex-bits ns)))

(defn ss [s offset length]
  (subs s offset (+ offset length)))

(declare parse-bits-packet)

(defn parse-bits-number [bits]
  (loop [collected []
         rest bits]
    (let [group (ss rest 0 5)
          fst (first group)
          r (subs group 1)
          collected' (conj collected r)
          rest' (subs rest 5)]
      (if (= \0 fst)
        [rest'
         [(parse-bin-int (apply str collected'))]]
        (recur collected' rest')))))

(defn parse-bits-op-number [bits]
  (let [bi-number (ss bits 0 11)
        number (parse-bin-int bi-number)]
    (reduce (fn [[to-parse parsed] i]
              (let [[r' pp] (parse-bits-packet to-parse)]
                [r' (conj parsed pp)]))
      [(subs bits 11) []]
      (range number))))

(defn parse-bits-op-length [bits]
  (let [bi-length (ss bits 0 15)
        length (parse-bin-int bi-length)
        rest-after-packet (subs bits (+ 15 length))]
    (loop [to-parse (ss bits 15 length)
           packets []]
      (let [[r' pp] (parse-bits-packet to-parse)
            p' (conj packets pp)]
        (if (str/blank? r')
          [rest-after-packet p']
          (recur r' p'))))))

(defn parse-bits-op [bits]
  (let [length-type-id (first bits)]
    (if (= \0 length-type-id)
      (parse-bits-op-length (subs bits 1))
      (parse-bits-op-number (subs bits 1)))))

(defn type-ids-parsers [ik]
  (if (= 4 ik)
    parse-bits-number
    parse-bits-op))

(def type-ids-ops
  [+
   *
   min
   max
   identity
   #(if (< %2 %1) 1 0)
   #(if (< %1 %2) 1 0)
   #(if (= %1 %2) 1 0)])

(defn parse-bits-packet [bits]
  (let [bi-version (ss bits 0 3)
        bi-type-id (ss bits 3 3)
        version (parse-bin-int bi-version)
        type-id (parse-bin-int bi-type-id)
        p-fn (type-ids-parsers type-id)
        to-parse (subs bits 6)
        [rest payload] (p-fn to-parse)]
    [rest {:version version
           :type-id (get type-ids-ops type-id)
           :payload payload}]))

(defn find-versions [tt]
  (->> tt
    (tree-seq map? :payload)
    (map :version)
    (remove nil?)))

(defn day-16-1 [data]
  (->> (hex-bits-s data)
    parse-bits-packet
    last
    find-versions
    (reduce +)))
(defn run-day-16-1 []
  (day-16-1 (str/trim (input 16))))

(defn bits-eval [node]
  (if-let [f (:type-id node)]
    (apply f (:payload node))
    node))

(defn day-16-2 [data]
  (->> (hex-bits-s data)
    parse-bits-packet
    last
    (clojure.walk/postwalk bits-eval)))
(defn run-day-16-2 []
  (day-16-2 (str/trim (input 16))))

(defn probe-fn [xvel yvel]
  (fn [step]
    (let [gravity (reduce + (range step))
          max-x-step (min step xvel)
          drag (reduce + (range max-x-step))]
      [(- (* max-x-step xvel) drag)
       (- (* step yvel) gravity)])))

(defn parse-probe-area [d]
  (let [parts (str/split (str/trim d) #"[. ,=]")
        gin #(parse-int (nth parts %))]
    {:xmin (gin 3)
     :xmax (gin 5)
     :ymin (gin 8)
     :ymax (gin 10)}))

(defn find-yvel [ymin]
  (dec (- ymin)))

(defn find-best-elevation [yvel]
  (reduce + (range yvel 0 -1)))

(defn day-17-1 [data]
  (-> data
    parse-probe-area
    :ymin
    find-yvel
    find-best-elevation))
(defn run-day-17-1 []
  (day-17-1 (str/trim (input 17))))

(defn before-or-in-area [{:keys [xmax ymin]} [x y]]
  (and
    (<= x xmax)
    (<= ymin y)))

(defn within-area [{:keys [xmax xmin ymax ymin]} [x y]]
  (and
    (<= xmin x xmax)
    (<= ymin y ymax)))

(defn generate-velocities [{:keys [xmax ymin]}]
  (for [xvel (range 0 (inc xmax))
        yvel (range ymin (inc (find-yvel ymin)))]
    [xvel yvel]))

(defn probe-trajectory-inf [[xvel yvel]]
  (map (probe-fn xvel yvel) (range)))

(defn probe-trajectory [target-area vels]
  (take-while
    (partial before-or-in-area target-area)
    (probe-trajectory-inf vels)))

(defn day-17-2 [data]
  (let [area (parse-probe-area data)]
    (->> area
      generate-velocities
      (map (partial probe-trajectory area))
      (keep #(some (partial within-area area) %))
      count)))

(defn run-day-17-2 []
  (day-17-2 (str/trim (input 17))))

(defn update-last [v f]
  (update v (dec (count v)) f))

(defn snail>-< [to-scan collected current next next-next]
  (cond-> {:to-scan to-scan :collected collected}
    (not-empty collected) ;; has numbers on the left
    (update :collected  #(update-last %
                           (fn [{:keys [n d]}]
                             {:n (+ n (:n current))
                              :d d})))
    true                    ;; always add 0 in place of pair
    (update :collected #(conj % {:n 0 :d (-> current :d dec)}))
    true                    ;; drop this and right number from the pair
    (update :to-scan #(drop 2 %))
    (some? next-next)        ;; if smth is on the right of the pair
    (->
      (update :collected #(conj % {:n (+ (:n next) (:n next-next))
                                   :d (:d next-next)}))
      (update :to-scan rest) ;; also update to-scan by dropping nn
      )))

(defn snail<-> [{:keys [n d]}]
  [{:n (-> n (/ 2) Math/floor int) :d (inc d)}
   {:n (-> n (/ 2) Math/ceil int) :d (inc d)}])

(defn snail-reduction-step [a]
  (let [[normal1 too-deep] (split-with #(>= 4 (:d %)) a)
        [normal2 too-big] (split-with #(> 10 (:n %)) a)]
    (cond
      (not-empty too-deep) ;; needs explode
      (let [{:keys [to-scan collected]}
            (snail>-< too-deep
              (vec normal1)
              (first too-deep)
              (second too-deep)
              (nth too-deep 2 nil))]
        (into collected to-scan))
      (not-empty too-big) ;; needs split
      (into (vec normal2) (concat (snail<-> (first too-big)) (rest too-big)))
      :else ;; good
      a)))

(defn snail-reduce [a]
  (reduce #(if (= %1 %2) (reduced %1) %2)
    (iterate snail-reduction-step a)))

(defn tree-seq-depth [branch? children root]
  (let [walk (fn walk [depth node]
               (lazy-seq
                 (cons {:n node :d depth}
                   (when (branch? node)
                     (mapcat (partial walk (inc depth)) (children node))))))]
    (walk 0 root)))

(defn snail-parse [sn]
  (->> sn
    read-string
    (tree-seq-depth sequential? seq)
    (filter (comp int? :n))))

(defn snail+ [a b]
  (concat
    (map #(update % :d inc) a)
    (map #(update % :d inc) b)))
(defn snail+red [a b]
  (snail-reduce (snail+ a b)))
(defn snail++ [& xs]
  (reduce snail+red xs))
(defn psnail++ [& xs]
  (reduce snail+red (map snail-parse xs)))

(defn mag-calc [[a b]]
  (if (nil? b)
    a
    {:n (+ (* (:n a) 3) (* (:n b) 2))
     :d (dec (:d a))}))

(defn mag-one-depth [m ad]
  (let [gd (:d (first ad))
        ps (partition 2 2 nil ad)]
    (if (= m gd)
      (mapv mag-calc ps)
      ad)))

(defn magnitude-step [a]
  (let [by-depth (partition-by :d a)
        m (:d (apply max-key :d a))]
    (mapcat (partial mag-one-depth m) by-depth)))

(defn magnitude [a]
  (loop [r a]
    (if (= 1 (count r))
      (:n (first r))
      (recur (magnitude-step r)))))

(defn day-18-1 [data]
  (->> data
    (map snail-parse)
    (apply snail++)
    magnitude))
(defn run-day-18-1 []
  (day-18-1 (inp-lines 18)))

(defn day-18-2 [data]
  (let [numbers (map snail-parse data)]
    (reduce max (for [x numbers
                     y numbers]
                 (let [xy (magnitude (snail++ x y))

                       yx (magnitude (snail++ y x))]
                   (max xy yx))))))
(defn run-day-18-2 []
  (day-18-2 (inp-lines 18)))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn rotations [[x y z]]
  [[x y z]
   [(- y) x z]
   [(- x) (- y) z]
   [y (- x) z]])
(defn z-orientations [[x y z]]
  [[x y z]
   [x z (- y)]
   [x (- y) (- z)]
   [x (- z) y]
   [(- z) y x]
   [z y (- x)]])
(defn orientations [[x y z]]
  (->> (z-orientations [x y z])
    (mapv rotations)
    (apply concat)
    vec))

(defn parse-scanners [data]
  (->> data
    (remove str/blank?)
    (partition-by #(str/starts-with? % "---"))
    (remove #(= 1 (count %)))
    (mapv #(mapv comma-ints %))))

(defn prepare-scanners [scanners]
  (map-indexed (fn [idx s]
                 {:beacons nil
                  :pos nil
                  :n idx
                  :orientations (transpose (map orientations s))})
    scanners))

(defn scanners-find-overlap** [unknown known]
  (let [possible-orientations (:orientations unknown)
        known-beacons (:beacons known)]
    (for [orientation possible-orientations
          beacon known-beacons
          single orientation
          :let [diff (mapv - beacon single)
                shifted (set (map #(mapv + % diff) orientation))]]
      {:overlap (clojure.set/intersection shifted known-beacons)
       :shifted shifted
       :pair (:n known)
       :pos diff})))

(defn scanners-find-overlap* [unknown known]
  (->>
    (scanners-find-overlap** unknown known)
    (drop-while (fn [{:keys [overlap]}]
                  (< (count overlap) 12)))
    first))
;; mostly for part1 vs part2
(def scanners-find-overlap (memoize scanners-find-overlap*))

(defn pair-scanner* [unknown known-scanners]
  (reduce (fn [_ known]
            (when-let [overlap (scanners-find-overlap unknown known)]
              (reduced overlap)))
    nil known-scanners))

(defn pair-scanner [unknown known-scanners]
  (when-let [overlap (pair-scanner* unknown known-scanners)]
    {:pos          (:pos overlap)
     :beacons      (:shifted overlap)
     :n            (:n unknown)
     :orientations (:orientations unknown)}))

(defn scanners-init [{:keys [orientations n]}]
  {:beacons (set (first orientations))
   :pos [0 0 0]
   :n n
   :orientations orientations})

(defn pair-all-scanners [scanners]
  (loop [to-scan (rest scanners)
         all-known [(-> scanners first scanners-init)]]
    (if (empty? to-scan)
      all-known
      (let [unknown (first to-scan)
            to-scan' (rest to-scan)
            maybe-known (pair-scanner unknown all-known)]
        (if maybe-known
          (recur to-scan' (conj all-known maybe-known))
          (recur (concat to-scan' (list unknown)) all-known))))))

(defn day-19-1 [data]
  (->> data
    parse-scanners
    prepare-scanners
    pair-all-scanners
    (map :beacons)
    (reduce clojure.set/union)
    count))
(defn run-day-19-1 []
  (day-19-1 (inp-lines 19)))

(defn scanner-distance [s1 s2]
  (let [pos1 (:pos s1)
        pos2 (:pos s2)
        diffs (mapv - pos1 pos2)
        abs (mapv #(Math/abs %) diffs)]
    (reduce + abs)))

(defn scanners-distances [scanners]
  (for [s1 scanners
        s2 scanners]
    (scanner-distance s1 s2)))

(defn day-19-2 [data]
  (->> data
    parse-scanners
    prepare-scanners
    pair-all-scanners
    scanners-distances
    (reduce max)))
(defn run-day-19-2 []
  (day-19-2 (inp-lines 19)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
