(ns aoc-2021.day04
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [blank? trim]]
   [clojure.test :refer :all]))

(defn build-boards [lines]
  (->> lines
       (filter #(not (blank? %1)))
       (map #(parse-int-array (trim %1) " +"))
       (partition 5)))

(defn puzzle-input [stream]
  (let [[rnd-num-line & boards-lines] (line-seq stream)]
    { :random-numbers (parse-int-array rnd-num-line)
     :boards (build-boards boards-lines)}))

;; part 1

(defn transpose [board]
  (apply map vector board))

(defn remove-number [board number]
  (map #(filter (comp not #{number}) %1) board))

(defn win? [board]
  (= [] (find-first empty? board)))

(defn score [number board]
  (* number (reduce + (apply concat board))))

(defn find-winning-board [boards numbers]
  (reduce
    (fn [boards number]
      (let [new-boards (map #(remove-number %1 number) boards)
            winner (find-first win? new-boards)]
        (if winner
          (reduced [number winner])
          new-boards)))
    (concat boards (map transpose boards))
    numbers))

(defpart part1 [input]
  (->> (find-winning-board (input :boards) (input :random-numbers))
       (apply score)))

;; part 2

(defn remove-number-2 [board number]
  (if-let [[x y] (get-in board [:board number])]
    (-> board
        (update-in [:board] dissoc number)
        (update-in [:completed x] (fnil inc 0))
        (update-in [:completed (- (inc y))] (fnil inc 0)))
    board))

(defn win-2? [board]
  (-> (some #(>= % 5) (vals (:completed board)))
      boolean))

(defn play [boards numbers]
  (reductions
    (fn [{:keys [boards]} number]
      (let [{winners true, non-winners false} (group-by win-2? (map #(remove-number-2 % number) boards))]
        {:boards non-winners
         :winners winners
         :number number}))
    {:boards boards}
    numbers))

(defn init-board [input]
  {:board (into {} (mapcat
                     (fn [row y]
                       (map-indexed (fn [x n] [n [x y]]) row))
                     input (range)))
   :completed {}})

(defn find-last-winning-board [boards numbers]
  (let [{last-winners :winners, number :number}
        (->> (play boards numbers)
             (filter #(not-empty (:winners %)))
             last)]
    [number (first last-winners)]))

(defn score-2 [number board]
  (* number (reduce + (keys (board :board)))))

(defpart part2 [input]
  (->> (find-last-winning-board (map init-board (input :boards)) (input :random-numbers))
       (apply score-2)))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest remove-number-test
  (are [board number expected] (= expected (remove-number board number))
    [[0 1] [3 4]] 5 [[0 1] [3 4]]
    [[0 1] [3 4]] 0 [[1] [3 4]]))

(deftest win?-test
  (are [board expected] (= expected (win? board))
    [[] [1 2]] true
    [[1] []] true
    [[0] [1 2]] false))

(deftest find-winning-board-test
  (are [boards numbers expected] (= expected (find-winning-board boards numbers))
    [[[0 1] [2 3]] [[4 5] [6 7]]] [1 4 5 7 0 3 2] [5 [[] [6 7]]]))

(deftest part1-test
  (is (= 4512 (part1 test-data))))

(deftest remove-number-2-test
  (are [board number] (= board (remove-number-2 board number))
    {:board {0 [0 0], 1 [0 1], 2 [1 0], 3 [1 1]} :completed {}}
    5)
  (are [board number expected] (= expected (remove-number-2 board number))
    {:board {0 [0 0], 1 [0 1], 2 [1 0], 3 [1 1]} :completed {}}
    3
    {:board {0 [0 0], 1 [0 1], 2 [1 0]} :completed {1 1, -1 1}}

    {:board {0 [0 0], 1 [0 1], 2 [1 0]} :completed {1 1, -1 1}}
    1
    {:board {0 [0 0], 2 [1 0]} :completed {0 1, 1 1, -1 2}}))

(deftest win-2?-test
  (are [board] (not (win-2? board))
    {:completed {}} 
    {:completed {1 2, -1 4}})
  (are [board] (win-2? board)
    {:completed {1 2, -1 5}}))

(deftest init-board-test
  (is (= {:board {0 [0 0], 1 [1 0], 2 [0 1], 3 [1 1]}, :completed {}}
         (init-board [[0 1] [2 3]]))))

(deftest part2-test
  (is (= 1924 (part2 test-data))))
