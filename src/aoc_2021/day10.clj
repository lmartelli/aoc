(ns aoc-2021.day10
  (:require
   [aoc.core :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-lines stream))

;; part 1

(def closing {\[ \] \( \) \{ \} \< \>})
(def opening (into #{} (keys closing)))

(defn validate [line]
  (loop [line line, opened []]
    (if (zero? (count line))
      (if (zero? (count opened))
        :legal
        [:incomplete opened])
      (let [c (first line)]
        (cond
          (opening c) (recur (rest line) (conj opened c))
          (= c (closing (peek opened))) (recur (rest line) (pop opened))
          :else [:corrupted c])))))

(defpart part1 [input]
  (->> input
       (map validate)
       (filter #(and (vector? %) (= :corrupted (first %))))
       (map second)
       (map {\) 3, \] 57, \} 1197, \> 25137})
       (reduce +)))

;; part 2

(defn middle-score [scores]
  (->> (sort scores)
       (drop (quot (count scores) 2))
       first))

(defpart part2 [input]
  (->> input
       (map validate)
       (filter #(and (vector? %) (= :incomplete (first %))))
       (map (comp reverse second))
       (map #(reduce (fn [score c] (+ ({\( 1, \[ 2, \{ 3, \< 4} c) (* 5 score))) 0 %))
       middle-score))

;; tests

(def test-data (puzzle-input (test-input *ns*)))

(deftest validate-test
  (are [input] (= :legal (validate input))
    ""
    "()" "[]" "{}" "<>"
    "([])" "[[]]" "{[]}" "<[]>"
    "([])" "{()()()}" "<([{}])>"
    "[<>({}){}[([])<>]]"
    "(((((((((())))))))))"
    "()[]"
    )
  (are [input opened] (= [:incomplete (vec opened)] (validate input))
    "(" "("
    "{" "{"
    "[" "["
    "<" "<"
    "(()" "("
    "[({(<(())[]>[[{[]{<()<>>" "[({([[{{"
    "[(()[<>])]({[<{<<[]>>(" "({[<{("
    "<{([{{}}[<[[[<>{}]]]>[]]" "<{([")
  (are [input illegal-char] (= [:corrupted illegal-char] (validate input))
    ")" \)
    "}" \}
    "]" \]
    ">" \>
    "(>" \>
    "(]" \]
    "(}" \}
    "{)" \)
    "{]" \]
    "[)" \)
    "[}" \}
    "[>" \>
    "((]))" \]
    "(([))" \)
    "{()()()>" \>
    "(((()))}" \}
    "<([]){()}[{}])" \)
    "{([(<{}[<>[]}>{[]{[(<()>" \}
    "[[<[([]))<([[{}[[()]]]" \)
    "[{[{({}]{}}([{[{{{}}([]" \]
    "[<(<(<(<{}))><([]([]()" \)
    "<{([([[(<>()){}]>(<<{{" \>))

(deftest part1-test
  (is (= 26397 (part1 test-data))))

(deftest middle-score-test
  (are [scores expected] (= expected (middle-score scores))
    [1] 1
    [1 3 2] 2
    [288957 5566 1480781 995444 294] 288957
    [5 4 3 2 1] 3))

(deftest part2-test
  (is (= 288957 (part2 test-data))))
