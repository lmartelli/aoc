(ns aoc-2021.day21
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-parse-lines
    stream
    (fn [line]
      (-> (re-find #": (\d+)" line)
          (nth 1)
          parse-int))))

;; part 1

(def deterministic-die (cycle (range-inc 1 100)))

(defn init-state [positions dice]
  {:turn 0
   :players (mapv (fn [pos] {:position pos :score 0}) positions)
   :dice dice})

(defn update-player [{:keys [position score]} played-dice]
  (let [new-position (-> (+ position played-dice)
                         dec
                         (mod 10)
                         inc)]
    {:position new-position
     :score (+ score new-position)}))

(defn play-1-turn [{:keys [turn players dice]}]
  (let [[played-dice new-dice] (split-at 3 dice)
        current-player (mod turn 2)]
    {:turn (inc turn)
     :players (update players current-player update-player (reduce + played-dice))
     :dice new-dice}))

(defn play [state]
  (iterate play-1-turn state))

(defn wins? [player]
  (-> (player :score)
      (>= 1000)))

(defn end? [{:keys [players]}]
  (some wins? players))

(defn looser [{:keys [players]}]
  (find-first (complement wins?) players))

(defn play-game [init-positions dice]
  (->> (play (init-state init-positions dice))
       (find-first end?)))

(defpart part1 [init-positions]
  (let [end-state (play-game init-positions deterministic-die)]
    (* (:score (looser end-state)) (* 3 (end-state :turn)))))

;; part 2

(def dirac-die-proba
  {3 1,
   4 3,
   5 6,
   6 7,
   7 6,
   8 3,
   9 1})

(defn next-player [current-player]
  (-> (inc current-player) (mod 2)))

(defn wins-dirac? [player]
  (-> (player :score)
      (>= 21)))

(defn winner [[p0 p1]]
  (cond
    (wins-dirac? p0) :player1
    (wins-dirac? p1) :player2))

(defn play-die [players current-player die]
  (let [new-players (update players current-player update-player die)]
    (or (winner new-players)
        new-players)))

(defn gen-universes [universe universe-proba current-player]
  (->> (map (fn [[played-dice dice-proba]]
              {(play-die universe current-player played-dice) (* universe-proba dice-proba)})
            dirac-die-proba)
       (reduce (partial merge-with +))))
  
(defn play-1-turn-dirac [[current-player universes]]
  [(next-player current-player)
   (reduce
     #(merge-with + %1 %2)
     (map (fn [[universe universe-proba]]
            (if (#{:player1 :player2} universe)
              {universe universe-proba}
              (gen-universes universe universe-proba current-player)))
          universes))])

(defn end-dirac? [universes]
  (and (= 2 (count universes))
       (universes :player1)
       (universes :player2)))

(defn play-dirac [[current-player universes]]
  (println "nb ≠ universes" (count universes)
           "total universes" (->> universes vals (reduce +))
           "player1 wins" (universes :player1)
           "player2 wins" (universes :player2))
  (if (end-dirac? universes)
    universes
    (play-dirac (play-1-turn-dirac [current-player universes]))))

(defpart part2 [positions]
  (->> (play-dirac [0 {[{:position (positions 0) :score 0} {:position (positions 1) :score 0}] 1}])
       vals
       (apply max)))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest play-test
  (let [expected [{:turn 0
                   :players        [{:position 4 :score 0}
                                    {:position 8 :score 0}]
                   }
                  {:turn 1
                   :players        [{:position 10 :score 10}
                                    {:position 8 :score 0}]
                   }
                  {:turn 2
                   :players        [{:position 10 :score 10}
                                    {:position 3 :score 3}]
                   }
                  {:turn 3
                   :players        [{:position 4 :score 14}
                                    {:position 3 :score 3}]
                   }
                  {:turn 4
                   :players        [{:position 4 :score 14}
                                    {:position 6 :score 9}]
                   }]]
    (is (= expected
         (->> (play (init-state test-data deterministic-die)) (take (count expected)) (map #(dissoc % :dice)))))))

(deftest part1-test
  (is (= 739785 (part1 test-data))))

(deftest part2-test
  (is (= 444356092776315 (part2 test-data))))
