(ns aoc-2021.day23
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)))

;; part 1

(defn parse-diagram [lines]
  (->> (array-2d-to-map (set (range 4)) (char-offset \A) lines)
       (sort-by key #(compare %2 %1))
       (reduce
         (fn [rooms [[x y] amphipod]]
           (update rooms (-> x (- 3)  (/ 2)) (fnil conj []) amphipod))
         {})))

(def empty-hallway (zipmap [0 1 3 5 7 9 10] (repeat nil)))

(defn x-pos [[type n]]
  (case type
    :room (+ 2 (* 2 n))
    :hallway n))

(defn passage-free? [{:keys [from to]} hallway]
  (let [[x-min x-max] (min-max (x-pos from) (x-pos to))]
    (not-any? #(< x-min % x-max) (keep #(if (val %) (key %)) hallway))))

(defn amphipod-moves [rooms hallway]
  (let [[dest-rooms src-rooms] (->> (group-by-pred
                                      (fn [[n amphipods]] (every? #{n} amphipods))
                                      rooms)
                                    (map #(into {} %)))]
    (->> (concat
           (apply concat
             (keep
               (fn [[n amphipods]]
                 (let [amphipod (peek amphipods)]
                   (if (dest-rooms amphipod)
                     [{:amphipod amphipod :from [:room n] :to [:room amphipod]}]
                     (map
                       (fn [hallway-dest]
                         {:amphipod amphipod :from [:room n] :to [:hallway hallway-dest]})
                       (keep #(if (nil? (val %)) (key %)) hallway)))))
               src-rooms))
           (keep
             (fn [[x amphipod]]
               (if (dest-rooms amphipod)
                 {:amphipod amphipod :from [:hallway x] :to [:room amphipod]}))
             hallway))
         (filter #(passage-free? % hallway)))))

(defn all-in-room-ok? [[n amphipods]]
  (apply = n amphipods))

(defn finished? [rooms hallway]
  (and (every? nil? (vals hallway))
       (every? all-in-room-ok? rooms )))

(defn dist-to-hallway [[location arg] rooms room-size]
  (case location
    :room (- (inc room-size) (count (rooms arg)))
    :hallway 0))

(defn dist-to-room [[location arg] rooms room-size]
  (case location
    :room (- room-size (count (rooms arg)))
    :hallway 0))

(defn dist [{:keys [from to]} rooms room-size]
  (+ (dist-to-hallway from rooms room-size)
     (dist-to-room to rooms room-size)
     (abs (- (x-pos from) (x-pos to)))))

(defn remove-amphipod-at [state [location arg]]
  (case location
    :room (update-in state [:rooms arg] pop)
    :hallway (assoc-in state [:hallway arg] nil)))

(defn set-amphipod-at [state [location arg] amphipod]
  (case location
    :room (update-in state [:rooms arg] conj amphipod)
    :hallway (assoc-in state [:hallway arg] amphipod)))

(defn cost [{:keys [amphipod from to] :as move} rooms hallway room-size]
  (* (dist move rooms room-size)
     (case (int amphipod)
       0 1
       1 10
       2 100
       3 1000)))

(defn apply-move [state {:keys [amphipod from to] :as move}]
  (-> state
      (remove-amphipod-at from)
      (set-amphipod-at to amphipod)))

(defn to-room? [{[location] :to}]
  (= location :room))

(defn moves-heuristic [moves rooms hallway room-size]
  (if-let [to-room (find-first to-room?  moves)]
    [to-room]
    (->> moves
         (map #(assoc % :cost (cost % rooms hallway room-size)))
         (sort-by #(vector (- (:amphipod %)) (:cost %))))))

(defn find-lowest-energy-solution
  ([rooms] (find-lowest-energy-solution
             {:rooms rooms
              :hallway empty-hallway
              :room-size (count (second (first rooms)))
              :current-cost 0}
             ##Inf))
  ([{:keys [rooms hallway room-size current-cost] :as state} current-best]
   (cond
     (finished? rooms hallway)
     (do (if (< current-cost current-best)
           (println "New best:" current-cost)
           #_(println current-cost))
         (min current-cost current-best))
     (> current-cost current-best)
     (do #_(println "Abandon" current-cost ">" current-best)
         current-best)
     :else
     (let [moves (amphipod-moves rooms hallway)]
       (if (empty? moves)
         (do #_(println "Dead-end")
             current-best)
         (reduce
           (fn [current-best move]
             (find-lowest-energy-solution
               (-> state
                   (update :current-cost + (cost move rooms hallway room-size))
                   (apply-move move))
               current-best))
           current-best
           (moves-heuristic moves rooms hallway room-size)))))))

(defpart part1 [diagram]
  (find-lowest-energy-solution (parse-diagram diagram)))

;; part 2

(defpart part2 [diagram]
  (find-lowest-energy-solution
    (parse-diagram
      (concat
        (take 3 diagram)
        ["  #D#C#B#A#"
         "  #D#B#A#C#"]
        (drop 3 diagram)))))

;; tests

(deftest part1-test (part-test part1 12521))

(deftest part2-test)

(deftest amphipod-moves-test
  (testing "Amphipod moves to his room as soon as possible"
    (are [rooms hallway expected] (= expected (amphipod-moves rooms hallway))
      {0 [1],   1 [],  2 []}  {0 nil}  [{:amphipod 1 :from [:room 0] :to [:room 1]}]
      {0 [1 2], 1 [],  2 []}  {0 nil}  [{:amphipod 2 :from [:room 0] :to [:room 2]}]
      {0 [1],   1 [1], 2 []}  {0 nil}  [{:amphipod 1 :from [:room 0] :to [:room 1]}]
      {0 [],    1 [],  2 []}  {0 0}    [{:amphipod 0 :from [:hallway 0] :to [:room 0]}]
      {0 [],    1 [2], 2 [1]} {0 0 10 nil} [{:amphipod 0 :from [:hallway 0] :to [:room 0]}]
      ))
  (testing "An amphipod cannot move to his room if it is occupied by other amphipod type"
    (are [rooms hallway expected] (= expected (amphipod-moves rooms hallway))
      {0 [1],   1 [0], 2 []}  {}     []
      {0 [1 0], 1 [0], 2 []}  {0 0}  []
    ))
  (testing "An amphipod in the hallway may block the passage"
    (are [rooms hallway expected] (= expected (amphipod-moves rooms hallway))
      {0 [1], 1 [], 2 []}   {3 0}  []
      {0 [2], 1 [], 2 []}   {3 0}  []
      {0 [2], 1 [], 2 []}   {5 0}  []
      {0 [2], 1 [], 2 []}   {0 1, 3 0}  []
      ))
  (testing "An amphipod in the hallway may not block the passage"
    (are [rooms hallway expected] (= expected (amphipod-moves rooms hallway))
      {0 [1], 1 [], 2 []}   {1 0}  [{:amphipod 1 :from [:room 0] :to [:room 1]}]
      {0 [1], 1 [], 2 []}   {5 0}  [{:amphipod 1 :from [:room 0] :to [:room 1]}]
      {0 [2], 1 [], 2 []}   {1 0}  [{:amphipod 2 :from [:room 0] :to [:room 2]}]
      {0 [2], 1 [], 2 []}   {7 0}  [{:amphipod 2 :from [:room 0] :to [:room 2]}]
      ))
  (testing "An amphipod may move to the hallway if not in already in his room"
    (are [rooms hallway expected] (= (set expected) (set (amphipod-moves rooms hallway)))
      {0 [1], 1 [0], 2 []}   {0 nil} [{:amphipod 1 :from [:room 0] :to [:hallway 0]}
                                      {:amphipod 0 :from [:room 1] :to [:hallway 0]}]
      {0 [1], 1 [0], 2 []}   {0 nil, 1 nil} [{:amphipod 1 :from [:room 0] :to [:hallway 0]}
                                             {:amphipod 0 :from [:room 1] :to [:hallway 0]}
                                             {:amphipod 1 :from [:room 0] :to [:hallway 1]}
                                             {:amphipod 0 :from [:room 1] :to [:hallway 1]}]      
      ))
  )

(deftest dist-test
  (testing "Room to room"
    (are [from to rooms room-size expected] (= expected
                                               (cost {:from from, :to to} rooms room-size)
                                               (cost {:from to, :to from} rooms room-size))
      [:room 1] [:room 0] {0 [], 1 [0] } 2 6
      [:room 2] [:room 0] {0 [], 2 [0] } 2 8
      [:room 2] [:room 0] {0 [0], 2 [1 0] } 4 10
      ))
  (testing "Room to hallway"
    (are [from to rooms room-size expected] (= expected (cost {:from from, :to to} rooms room-size))
      [:room 0] [:hallway 0] {0 [1] } 2 4
      [:room 0] [:hallway 1] {0 [1] } 2 3
      [:room 0] [:hallway 3] {0 [1] } 2 3
      [:room 0] [:hallway 10] {0 [1] } 2 10
      [:room 3] [:hallway 0] {3 [1] } 2 10
      [:room 0] [:hallway 0] {0 [1] } 4 6      
      ))
    (testing "Hallway to room"
    (are [from to rooms room-size expected] (= expected (cost {:from from, :to to} rooms room-size))
      [:hallway 0] [:room 0] {0 [] } 2 4
      [:hallway 0] [:room 0] {0 [1] } 2 3
      [:hallway 1] [:room 0] {0 [1] } 2 2
      [:hallway 3] [:room 0] {0 [1] } 2 2
      [:hallway 10] [:room 0] {0 [1] } 2 9
      [:hallway 0] [:room 3] {3 [1] } 2 9
      [:hallway 0] [:room 0] {0 [1] } 4 5      
      )))
