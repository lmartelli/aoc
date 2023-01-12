(ns aoc-2018.day15
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (vec (map vec (line-seq stream))))

;; part 1

(def item->type {\# :wall, \. :free, \E :elf, \G :goblin})
(def type->item (clojure.set/map-invert item->type))

(defn hit-points-row [state row]
  (->> (filter (fn [{[r c] :pos}] (= row r)) (vals (state :units)))
       (map (fn [{:keys [type hit-points]}]
              (format "%s(%d)" (type->item type) hit-points)))
       (str/join ", ")))

(defn printable-cave-map [state]
  (map str/join (state :cave-map)))

(defn printable-state [state]
  (map-indexed
    #(format "%s   %s" %2 (hit-points-row state %1))
    (map str/join (state :cave-map))))

(defn print-state [state]
  (doseq [line (printable-state state)]
    (println line)))

(defn unit? "Tells wether a map item is a unit"
  [item]
  (or (= \E item) (= \G item)))

(defn item-at [state [row col]]
  (get-in state [:cave-map row col]))

(defn set-at [state [row col] item]
  (assoc-in state [:cave-map row col] item))

(defn unit-at [state pos]
  (get-in state [:units pos]))

(defn init-state
  ([input] (init-state input (constantly 3)))
  ([input attack-power]
   (let [state {:cave-map input}
         init-unit (fn [[pos item]]
                     (if (unit? item)
                       {:pos pos
                        :type (item->type item)
                        :hit-points 200
                        :attack-power (attack-power (item->type item))}))]
     (assoc state
            :units (->> (keep init-unit (s2/row-col-and-values-seq (state :cave-map)))
                        (map (juxt :pos identity))
                        (into (sorted-map)))))))

(defn neighbours [[row col :as pos]]
  [[(dec row) col] [row (dec col)] [row (inc col)] [(inc row) col]])

(defn free? [cave-map pos]
  (=\. (get-in cave-map pos)))

(defn units [state]
  (vals (state :units)))

(defn get-targets [state unit-type]
  (->> (units state)
       (filter #(not= unit-type (:type %)))
       (map :pos)))

(defn free-neighbours [cave-map pos]
  (filter #(free? cave-map %) (neighbours pos)))

(defn get-destinations "Possible destinations, in reading order"
  [{:keys [cave-map] :as state} unit-type]
  (let [targets (get-targets state unit-type)]
    (into (sorted-set) (mapcat #(free-neighbours cave-map %) targets))))

(defn targets-in-range [state {:keys [pos type] :as unit}]
  (keep #(if-let [target (unit-at state %)]
           (if (not= type (target :type))
             target))
        (neighbours pos)))

(defn has-target-in-range? [state unit]
  (not-empty (targets-in-range state unit)))

(defn build-path [dest visited]
  (loop [cur dest
         path (list dest)]
    (if-let [prev (visited cur)]
      (recur prev (conj path prev))
      path)))

(defn min-pos [a b]
  (cond
    (nil? a) b
    (nil? b) a
    (pos? (compare a b)) b
     :else a))

(defn bfs-path [&{:keys [start neighbours destinations]}]
  (loop [last-visited #{start}
         visited {start nil}]
    (if-let [dest (some last-visited destinations)]
      (build-path dest visited)
      (if (empty? last-visited)
        nil
        (recur
          (set (remove #(contains? visited %) (mapcat neighbours last-visited)))
          (reduce
            (fn [new-visited cur-pos]
              (reduce
                (fn [new-visited next-pos]
                  (update new-visited next-pos min-pos cur-pos))
                new-visited
                (remove #(contains? visited %) (neighbours cur-pos))))
            visited
            last-visited))))))

(defn get-move [state unit]
  (if-not (has-target-in-range? state unit)
    (let [destinations (get-destinations state (unit :type))]
      (if-not (empty? destinations)
        (some-> (bfs-path :start (unit :pos)
                          :destinations destinations
                          :neighbours #(free-neighbours (state :cave-map) %))
                (nth 1))))))

(defn move [state {:keys [pos type] :as unit} to]
  (let [new-unit (assoc unit :pos to)]
    [(-> state
        (set-at pos \.)
        (set-at to (type->item type))
        (update :units dissoc pos)
        (assoc-in [:units to] (assoc unit :pos to)))
     new-unit]))

(defn try-move
  "Returns updated state *and* unit"
  [[state unit]]
  (if-let [to (get-move state unit)]
    (move state unit to)
    [state unit]))

(defn get-attack [state {unit-pos :pos :as unit}]
  (->> (targets-in-range state unit)
       (sort-by (juxt :hit-points :pos))
       first))

(defn attack [state assailant target]
  (if (> (target :hit-points) (assailant :attack-power))
    (update-in state [:units (target :pos) :hit-points] - (assailant :attack-power))
    (-> state
         (set-at (target :pos) \.)
         (update :units dissoc (target :pos)))))

(defn try-attack
  [[state unit]]
  (if-let [target (get-attack state unit)]
    (attack state unit target)
    state))

(defn turn [state unit]
  (let [targets (get-targets state (unit :type))]
    (if (empty? targets)
      (assoc state :end true)
      (-> [state unit]
          try-move
          try-attack))))

(defn alive? [state unit]
  (unit-at state (unit :pos)))

(defn round [state]
  (reduce
    (fn [state unit]
      #_(print-state state)
      #_(println "turn for" unit)
      (if (alive? state unit)
        (turn state unit)
        state))
    state
    (units state)))

(defn rounds [init-state]
  (iterate round init-state))

(defn combat-ends? [state]
  (state :end))

(defn outcome [nb-full-rounds state]
  (* nb-full-rounds
     (->> (state :units)
          vals
          (map :hit-points)
          (reduce +))))

(defn battle [init-state & {early-stop? :early-stop? :or {early-stop? (constantly false)}}]
  (let [[index final-state]  (first-index-and-val (some-fn early-stop? combat-ends?)
                                                  (rounds init-state))
        nb-full-rounds (dec index)]
    {:nb-full-rounds (dec index)
     :final-state final-state
     :outcome (outcome nb-full-rounds final-state)}))

(defpart part1 [input]
  (let [{:keys [final-state nb-full-rounds outcome]} (battle (init-state input))]
    (println "Battle ends after" nb-full-rounds "rounds")
    (print-state final-state)
    (println)
    outcome))

;; part 2

(defn count-units [state type]
  (->> (state :units)
       vals
       (map :type)
       frequencies
       type))

(defn find-min-attack-power [input]
  (let [nb-elves (count-units (init-state input) :elf)
        [attack-power battle-result]
        (algo/find-min-parameter
          3
          (fn [attack-power]
            (println "Trying Elves attack power" attack-power)
            (battle (init-state input #(if (= % :elf) attack-power 3))
                    :stop? #(< (count-units % :elf) nb-elves)))
          #(= nb-elves (count-units (% :final-state) :elf)))]
    (assoc battle-result :attack-power attack-power)))

(defpart part2 [input]
  (let [{:keys [attack-power final-state nb-full-rounds outcome]} (find-min-attack-power input)]
    (println "Elves attack power" attack-power)
    (println "Battle ends after" nb-full-rounds "rounds")
    (print-state final-state)
    outcome))

;; tests

(deftest part1-test)

(deftest part2-test)

(deftest init-state-test
  (are [cave-map expected] (= expected (init-state cave-map))
    ["###"
     ".EG"]
    {:cave-map ["###"
                ".EG"]
     :units {[1 1] {:pos [1 1], :type :elf,    :hit-points 200, :attack-power 3}
             [1 2] {:pos [1 2], :type :goblin, :hit-points 200, :attack-power 3}}}))

(defn test-state
  ([lines] (test-state lines {}))
  ([lines hit-points]
   (reduce
     (fn [state [pos points]]
       (assoc-in state [:units pos :hit-points] points))
     (init-state (parse-input-lines lines))
     hit-points)))

(deftest moves
  (let [data ["#######"
              "#E..G.#"
              "#...#.#"
              "#.G.#G#"
              "#######"]
        state (test-state data)
        at (fn [pos] (unit-at state pos))]
    (testing "Elves target goblins"
      (is (= #{[1 4] [3 2] [3 5]} (set (get-targets state :elf)))))
    (testing "Goblins target elves"
      (is (= #{[1 1]} (set (get-targets state :goblin)))))
    (testing "Units prioritize destinations in reading order"
      (are [target-type expected] (= expected (seq (get-destinations state target-type)))
        :elf [[1 3] [1 5] [2 2] [2 5] [3 1] [3 3]]
        :goblin [[1 2] [2 1]])))
  (testing "Unit with an enemy in range does not move"
    (let [state (test-state ["...."
                             ".GE."
                             "...."])]
      (are [pos] (nil? (get-move state (unit-at state pos)))
        [1 1]
        [1 2])))
  (testing "Units move 1 step to get closer to it's target, prioritizing by reading order"
    (let [state (test-state ["E....."
                             "....."
                             "G.E.G"])]
      (are [unit-pos expected] (= expected (get-move state (unit-at state unit-pos)))
        [2 2] [2 1]
        [2 0] [1 0]
        [2 4] [2 3]
        [0 0] [1 0]))))

(deftest attack-test
  (testing "A unit does not attack if it has no target in range"
    (let [state (test-state ["....."
                             ".G.E."
                             "....."])]
      (are [pos] (nil? (get-attack state (unit-at state pos)))
        [1 1]
        [1 3])))
  (testing "A unit attack the in-range target with the fewest hit points"
    (let [state (test-state [".G.."
                             ".EG."
                             ".G.."]
                            {[0 1] 5,
                             [1 2] 8
                             [2 1] 3})]
      (are [pos expected] (= expected (get-attack state (unit-at state pos)))
        [1 1] {:pos [2 1], :type :goblin, :hit-points 3,   :attack-power 3}
        [1 2] {:pos [1 1], :type :elf,    :hit-points 200, :attack-power 3}
        [0 1] {:pos [1 1], :type :elf,    :hit-points 200, :attack-power 3})))
  (testing "A unit attack the in-range target with the fewest hit points"
    (let [state (test-state [".G.."
                             ".EG."
                             ".G.."]
                            {[0 1] 3,
                             [1 2] 8
                             [2 1] 3})]
      (are [pos expected] (= expected (get-attack state (unit-at state pos)))
        [1 1] {:pos [0 1], :type :goblin, :hit-points 3,   :attack-power 3}))))

(deftest round-test
  (let [states
        (->> (scatter
               5
               ["#########" "#########" "#########" "#########" "#########"
                "#G..G..G#" "#.G...G.#" "#..G.G..#" "#.......#" "#.......#"
                "#.......#" "#...G...#" "#...G...#" "#..GGG..#" "#..GGG..#"
                "#.......#" "#...E..G#" "#.G.E.G.#" "#..GEG..#" "#..GEG..#"
                "#G..E..G#" "#.G.....#" "#.......#" "#G..G...#" "#G..G...#"
                "#.......#" "#.......#" "#G..G..G#" "#......G#" "#......G#"
                "#.......#" "#G..G..G#" "#.......#" "#.......#" "#.......#"
                "#G..G..G#" "#.......#" "#.......#" "#.......#" "#.......#"
                "#########" "#########" "#########" "#########" "#########"])
             (map test-state))]
    (doseq [[n n+1] (partition 2 1 states)]
      (is (= (printable-cave-map n+1) (printable-cave-map (round n))))))
  (let [s0 (test-state
             ["#######"
              "#.G...#"
              "#...EG#"
              "#.#.#G#"
              "#..G#E#"
              "#.....#"
              "#######"])
        expected-states
        {1 ["#######   "
            "#..G..#   G(200)"
            "#...EG#   E(197), G(197)"
            "#.#G#G#   G(200), G(197)"
            "#...#E#   E(197)"
            "#.....#   "
            "#######   "]
         2 ["#######   "
            "#...G.#   G(200)"
            "#..GEG#   G(200), E(188), G(194)"
            "#.#.#G#   G(194)"
            "#...#E#   E(194)"
            "#.....#   "
            "#######   "]
         23 ["#######   "
             "#...G.#   G(200)"
             "#..G.G#   G(200), G(131)"
             "#.#.#G#   G(131)"
             "#...#E#   E(131)"
             "#.....#   "
             "#######   "]
         24 ["#######   "
             "#..G..#   G(200)"
             "#...G.#   G(131)"
             "#.#G#G#   G(200), G(128)"
             "#...#E#   E(128)"
             "#.....#   "
             "#######   "]
         25 ["#######   "
             "#.G...#   G(200)"
             "#..G..#   G(131)"
             "#.#.#G#   G(125)"
             "#..G#E#   G(200), E(125)"
             "#.....#   "
             "#######   "]
         26 ["#######   "
             "#G....#   G(200)"
             "#.G...#   G(131)"
             "#.#.#G#   G(122)"
             "#...#E#   E(122)"
             "#..G..#   G(200)"
             "#######   "]
         27 ["#######   "
             "#G....#   G(200)"
             "#.G...#   G(131)"
             "#.#.#G#   G(119)"
             "#...#E#   E(119)"
             "#...G.#   G(200)"
             "#######   "]
         28 ["#######   "
             "#G....#   G(200)"
             "#.G...#   G(131)"
             "#.#.#G#   G(116)"
             "#...#E#   E(113)"
             "#....G#   G(200)"
             "#######   "]
         47 ["#######   "
             "#G....#   G(200)"
             "#.G...#   G(131)"
             "#.#.#G#   G(59)"
             "#...#.#   "
             "#....G#   G(200)"
             "#######   "]}
        states (iterate round s0)]
    (doseq [[n expected-state] expected-states]
      (is (= expected-state (printable-state (nth states n))) (str "State ater round " n)))
    (is (= 47 ((battle s0) :nb-full-rounds)))
    ))

(defn parse-battle-test [lines]
  (->> lines
       (map #(str/split % #"       |  -->  "))
       (apply map vector)))

(deftest battle-test
  (are [descr nb-full-rounds]
      (let [[start end] (parse-battle-test descr)]
        (is (= [nb-full-rounds end]
               ((juxt :nb-full-rounds (comp printable-state :final-state)) (battle (test-state start))))))
    ["#######       #######   "
     "#G..#E#       #...#E#   E(200)"
     "#E#E.E#       #E#...#   E(197)"
     "#G.##.#  -->  #.E##.#   E(185)"
     "#...#E#       #E..#E#   E(200), E(200)"
     "#...E.#       #.....#   "
     "#######       #######   "]
    37
    ["#######       #######   "
     "#E..EG#       #.E.E.#   E(164), E(197)"
     "#.#G.E#       #.#E..#   E(200)"
     "#E.##E#  -->  #E.##.#   E(98)"
     "#G..#.#       #.E.#.#   E(200)"
     "#..E#.#       #...#.#   "
     "#######       #######   "]
    46
    ["#######       #######   "
     "#E.G#.#       #G.G#.#   G(200), G(98)"
     "#.#G..#       #.#G..#   G(200)"
     "#G.#.G#  -->  #..#..#   "
     "#G..#.#       #...#G#   G(95)"
     "#...E.#       #...G.#   G(200)"
     "#######       #######   "]
    35
    ["#######       #######   "
     "#.E...#       #.....#   "
     "#.#..G#       #.#G..#   G(200)"
     "#.###.#  -->  #.###.#   "
     "#E#G#G#       #.#.#.#   "
     "#...#G#       #G.G#G#   G(98), G(38), G(200)"
     "#######       #######   "]
    54
    ["#########       #########   "
     "#G......#       #.G.....#   G(137)"
     "#.E.#...#       #G.G#...#   G(200), G(200)"
     "#..##..G#       #.G##...#   G(200)"
     "#...##..#  -->  #...##..#   "
     "#...#...#       #.G.#...#   G(200)"
     "#.G...G.#       #.......#   "
     "#.....G.#       #.......#   "
     "#########       #########   "]
    20))

(deftest min-attack-power-test
  (are [descr expected-outcome]
      (let [[start end] (parse-battle-test descr)]
        (is (= [end expected-outcome]
               (let [res (find-min-attack-power (parse-input-lines start))]
                 [(printable-state (res :final-state)) (res :outcome)]))))
    ["#######       #######   "
     "#.G...#       #..E..#   E(158)"
     "#...EG#       #...E.#   E(14)"
     "#.#.#G#  -->  #.#.#.#   "
     "#..G#E#       #...#.#   "
     "#.....#       #.....#   "
     "#######       #######   "]
    4988
    ["#######       #######   "
     "#E..EG#       #.E.E.#   E(200), E(23)"
     "#.#G.E#       #.#E..#   E(200)"
     "#E.##E#  -->  #E.##E#   E(125), E(200)"
     "#G..#.#       #.E.#.#   E(200)"
     "#..E#.#       #...#.#   "
     "#######       #######   "]
    31284
    ["#######       #######   "
     "#E.G#.#       #.E.#.#   E(8)"
     "#.#G..#       #.#E..#   E(86)"
     "#G.#.G#  -->  #..#..#   "
     "#G..#.#       #...#.#   "
     "#...E.#       #.....#   "
     "#######       #######   "]
    3478
    ["#######       #######   "
     "#.E...#       #...E.#   E(14)"
     "#.#..G#       #.#..E#   E(152)"
     "#.###.#  -->  #.###.#   "
     "#E#G#G#       #.#.#.#   "
     "#...#G#       #...#.#   "
     "#######       #######   "]
    6474
    ["#########       #########   "
     "#G......#       #.......#   "
     "#.E.#...#       #.E.#...#   E(38)"
     "#..##..G#       #..##...#   "
     "#...##..#  -->  #...##..#   "
     "#...#...#       #...#...#   "
     "#.G...G.#       #.......#   "
     "#.....G.#       #.......#   "
     "#########       #########   "]
    1140)) 

(deftest bfs-path-test
  (testing "Single path"
    (are [start destinations expected]
        (= expected (bfs-path :start start :destinations destinations :neighbours s2/direct-neighbours))
      [0 0] [[0 0]] [[0 0]]
      [0 0] [[0 1]] [[0 0] [0 1]]
      [0 0] [[0 2]] [[0 0] [0 1] [0 2]]))
  (testing "Prefer reading order if multiple directions are available"
    (are [start destinations expected]
        (= expected (bfs-path :start start :destinations destinations :neighbours s2/direct-neighbours))
      [0 0] [[1 1]] [[0 0] [0 1] [1 1]]
      [0 0] [[-1 -1]] [[0 0] [-1 0] [-1 -1]]
      [0 0] [[1 2]] [[0 0] [0 1] [0 2] [1 2]]))
  (testing "Choose 1st destination in the list"
    (are [start destinations expected]
        (= expected (bfs-path :start start :destinations destinations :neighbours s2/direct-neighbours))
      [0 0] [[0 2] [2 0]] [[0 0] [0 1] [0 2]]
      [0 0] [[2 0] [0 2]] [[0 0] [1 0] [2 0]]))
  (testing "Returns nil if no destination is reachable"
    (is (nil? (bfs-path :start [0 0] :destinations [[1 1] [2 2]] :neighbours (constantly []))))))
