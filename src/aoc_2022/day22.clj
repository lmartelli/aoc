(ns aoc-2022.day22
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as d2]
   [clojure.test :refer :all]))

(defn parse-path-element [s]
  (case s
    "R" :right
    "L" :left
    (parse-int s)))

(defn puzzle-input [stream]
  (let [[board [path]] (->> (line-seq stream) (split-seq empty?))]
    {:board board
     :path (->>  (re-seq #"\d+|[RL]" path)
                 (map parse-path-element))}))

;; part 1

(defn parse-board [lines]
  (array-2d-to-map #{:wall :open} {\# :wall \. :open} lines))

(defn print-board [board]
  (d2/print board {:wall \# :open \.})
  (println))

(defn print-state [{:keys [pos dir]} board]
  (-> board
      (assoc pos ({[1 0] \▶, [-1 0] \◀, [0 1] \▼, [0 -1] \▲ } dir))
      print-board))

(defn wrap [{:keys [pos dir] :as state} board]
  (assoc state :pos 
  (->> (iterate #(d2/- % dir) pos)
       (find-first (complement board))
       (d2/+ dir))))

(defn next-pos [{:keys [pos dir] :as state} board wrap]
  (let [next-pos (d2/+ pos dir)]
    (if (board next-pos)
      (assoc state :pos next-pos)
      (wrap state board))))

(defn step-forward [{:keys [pos dir] :as state} board wrap]
  (let [next-state (next-pos state board wrap)]
    (case (board (next-state :pos))
      :wall state
      :open next-state)))

(defn exec-path-element [{:keys [pos dir] :as state} e board wrap]
  #_(print-state state board)
  (if (number? e)
    (nth (iterate #(step-forward % board wrap) state) e)
    (update state :dir ({:right rotate-right, :left rotate-left} e))))

(defn walk-path [initial-state path board wrap]
  (reduce
    #(exec-path-element %1 %2 board wrap)
    initial-state
    path))

(defn password [{[x y] :pos, dir :dir}]
  (+ (* 1000 (inc y))
     (* 4 (inc x))
     ((->> (iterate rotate-right [1 0]) (map-indexed #(vector %2 %1)) (take 4) (into {})) dir)))

(defn start-pos [board-lines]
  [(count (re-find #" *" (first board-lines))) 0])

(defn solve [{:keys [board path]} wrap]
  (let [start (start-pos board)
        board (parse-board board)]
    (-> {:pos start :dir [1 0]}
        (walk-path path board wrap)
        (debug-val)
        password)))

(defpart part1 [input]
  (solve input wrap))

;; part 2

(def UP [0 -1])
(def DOWN [0 1])
(def LEFT [-1 0])
(def RIGHT [1 0])

(defn wrap-cube
  [{:keys [pos dir] :as state} board]
  (let [[x y] pos]
    ;;  12
    ;;  3
    ;; 45
    ;; 6
    (cond
      (and (= x  50) (<=   0 y  49) (= dir LEFT))  {:pos [  0 (- 149 y)] :dir RIGHT}  ;; 1 -> 4
      (and (= x   0) (<= 100 y 149) (= dir LEFT))  {:pos [ 50 (- 149 y)] :dir RIGHT}  ;; 4 -> 1

      (and (= x 149) (<=   0 y  49) (= dir RIGHT)) {:pos [ 99 (- 149 y)] :dir LEFT}   ;; 2 -> 5
      (and (= x  99) (<= 100 y 149) (= dir RIGHT)) {:pos [149 (- 149 y)] :dir LEFT}   ;; 5 -> 2

      (and (= y  49) (<= 100 x 149) (= dir DOWN))  {:pos [ 99 (- x 50)]  :dir LEFT}   ;; 2 -> 3
      (and (= x  99) (<=  50 y  99) (= dir RIGHT)) {:pos [(+ y 50)  49]  :dir UP}     ;; 3 -> 2

      (and (= x  50) (<=  50 y  99) (= dir LEFT))  {:pos [(- y 50)  100] :dir DOWN}    ;; 3 -> 4
      (and (= y 100) (<=   0 x  49) (= dir UP))    {:pos [50 (+ x 50)]   :dir RIGHT}   ;; 4 -> 3

      (and (= y   0) (<=  50 x  99) (= dir UP))    {:pos [0 (+ x 100)]   :dir RIGHT}   ;; 1 -> 6
      (and (= x   0) (<= 150 y 199) (= dir LEFT))  {:pos [(- y 100) 0]   :dir DOWN}    ;; 6 -> 1
      
      (and (= y   0) (<= 100 x 149) (= dir UP))    {:pos [(- x 100) 199] :dir UP}      ;; 2 -> 6
      (and (= y 199) (<=   0 x  49) (= dir DOWN))  {:pos [(+ x 100) 0]   :dir DOWN}    ;; 6 -> 2

      (and (= y 149) (<=  50 x  99) (= dir DOWN) ) {:pos [49 (+ x 100)]  :dir LEFT}    ;; 5 -> 6
      (and (= x  49) (<= 150 y 199) (= dir RIGHT)) {:pos [(- y 100) 149] :dir UP}      ;; 6 -> 5
      :else (throw (Exception. (str "Unhandled wrap position " pos)))
      )))

(defpart part2 [input]
  (solve input wrap-cube))

;; tests

(deftest exec-path-element-test
  (testing "Part1 - pacman  wrapping"
    (let [board (parse-board [".##"
                              "...#"
                              " ..."])]
      (testing "Move forward without any obstacle (forward and backward)"
        (are [from dir step to]
            (and (= {:pos to :dir dir} (exec-path-element {:pos from :dir dir} step board wrap))
                 (let [opposite-dir (d2/- dir)]
                   (= {:pos from :dir opposite-dir}
                      (exec-path-element {:pos to :dir opposite-dir} step board wrap))))
          [0 0] [0 1] 1 [0 1]
          [0 1] [1 0] 2 [2 1]
          ))
      (testing "Cannot move through walls"
        (are [from dir step to]
            (= {:pos to :dir dir} (exec-path-element {:pos from :dir dir} step board wrap))
          [0 0] [1 0] 1 [0 0]
          [0 1] [1 0] 3 [2 1] 
          [2 2] [0 -1] 2 [2 1]
          ))
      (testing "Wraps at boundaries"
        (are [from dir step to]
            (and (= {:pos to :dir dir} (exec-path-element {:pos from :dir dir} step board wrap))
                 (let [opposite-dir (d2/- dir)]
                   (= {:pos from :dir opposite-dir}
                      (exec-path-element {:pos to :dir opposite-dir} step board wrap))))
          [0 0] [0 -1] 1 [0 1]
          [1 2] [1 0] 4 [2 2]
          ))
      (testing "Cannot wrap through walls"
        (are [from dir step to]
            (= {:pos to :dir dir} (exec-path-element {:pos from :dir dir} step board wrap))
          [1 1] [-1 0] 2 [0 1]
          [1 2] [0 1] 2 [1 2])))))

(deftest password-test
  (is (= 6032 (password {:pos [6 8] :dir [1 0]}))))

(deftest part1-test (part-test part1 6032))

(deftest part2-test (part-test part2 5031))
