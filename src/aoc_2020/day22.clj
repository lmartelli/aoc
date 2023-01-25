(ns aoc-2020.day22
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(def-input-parser [lines]
  (->> (split-seq empty? lines)
       (map #(mapv parse-int (rest %)))
       (mapv queue)))

;; part 1

(defn round [decks]
  (let [[looser winner] (sort-by first decks)]
    [(conj (pop winner) (peek winner) (peek looser))
     (pop looser)]))

(defn game [decks]
  (->> (iterate round decks)
       (find-first (comp empty? second))))

(defn score [deck]
  (->> (map-indexed #(* (inc %1) %2) (reverse deck))
       (reduce +)))

(defpart part1 [decks]
  (-> decks
      game
      first
      score))

;; part 2

(defn peek-n-pop [coll]
  [(peek coll) (pop coll)])

(declare recursive-game)

(defn must-recurse? [decks]
  (every? #(>= (dec (count %)) (peek %))
          decks))

(defn copy-deck [[drawn & more :as deck]]
  (queue (take drawn more)))

(defn recursive-round [decks]
  #_(clojure.pprint/pprint decks)
  (let [winner (if (must-recurse? decks)
                 ((recursive-game (mapv copy-deck decks)) :player)
                 (if (apply > (map peek decks)) 0 1))
        looser (if (zero? winner) 1 0)]
    (-> (mapv pop decks)
        (update winner conj (peek (decks winner)) (peek (decks looser))))))

(defn recursive-game [decks]
  #_(println "*** new game")
  #_(clojure.pprint/pprint decks)
  (loop [[decks & more] (iterate recursive-round decks)
         history #{}]
    (cond
      (contains? history decks) (do #_(println "!!! Instant end !!!")
                                    {:player 0 :deck (first decks)})
      (empty? (decks 0)) {:player 1 :deck (decks 1)}
      (empty? (decks 1)) {:player 0 :deck (decks 0)}
      :else (recur
              more
              (conj history decks)))))

(defpart part2 [decks]
  (->> (recursive-game decks)
       :deck
       score))

;; tests

(deftest round-test
  (are [p1 p2 expected] (= expected
                           (round [(queue p1) (queue p2)])
                           (round [(queue p2) (queue p1)]))
    [0 3 9] [1 2 4] [[2 4 1 0] [3 9]]))

(def data
  ["Player 1:" "9" "2" "6" "3" "1"
   ""
   "Player 2:" "5" "8" "4" "7" "10"])

(deftest part1-test
  (test-with-lines part1 data 306))

(deftest recursive-round-test
  (testing "Normal round"
    (are [decks expected] (= expected (recursive-round (mapv queue decks)))
      [[9 2 6 3 1] [5 8 4 7 10]] [[2 6 3 1 9 5] [8 4 7 10]]))
  (testing "Recursive game"
    (are [decks expected] (= expected (recursive-round (mapv queue decks)))
      [[4 9 8 5 2] [3 10 1 7 6]] [[9 8 5 2] [10 1 7 6 3 4]])))

(deftest part2-test
  (test-with-lines part2 data 291))
