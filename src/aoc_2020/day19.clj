(ns aoc-2020.day19
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn unquote-string [s]
  (subs s 1 (dec (count s))))

(defn simplify-rule [rule]
  (cond
    (int? rule) rule
    (string? rule) rule
    :else (let [[type & sub-rules] rule]
            (if (= 1 (count sub-rules))
              (simplify-rule (first sub-rules))
              (concat [type] (map simplify-rule sub-rules))))))

(defn parse-rule [s]
  (let [[n & rule] (re-seq #"\d+|\"[a-z]+\"|\|" s)]
    [(parse-int n) (-> (concat [:or]
                               (->> (split-seq (eq "|") rule)
                                    (map (fn [tokens]
                                           (concat [:cat]
                                                   (map (partial parse-int-or unquote-string) tokens))))))
                       simplify-rule)]))

(defn simplify-rules [rules]
  (update-vals rules simplify-rule))

(def-input-parser [lines]
  (let [[rules messages] (split-seq empty? lines)]
    {:rules (into {} (map parse-rule) rules)
     :messages messages}))

;; part 1

(defn match [rule message]
  (cond
    (string? rule) (if (str/starts-with? message rule) rule nil)
    :else (let [[type & sub-rules] rule]
            (case type
              :or (some #(match % message) sub-rules)
              :cat (first
                        (reduce
                          (fn [[matched message] rule]
                            (if-let [sub-match (match rule message)]
                              [(str matched sub-match) (subs message (count sub-match))]
                              (reduced nil)))
                          [nil message]
                          sub-rules))))))

(defn match? [rule message]
  (if-let [matched (match rule message)]
    (= matched message)
    false))

(defn expand-rule [rules rule]
  (cond
    (int? rule) (expand-rule rules (rules rule))
    (string? rule) rule
    :else (cons (first rule)
                (map #(expand-rule rules %) (rest rule)))))

(defpart part1 [{:keys [rules messages]}]
  (let [rule-0 (expand-rule rules 0)]
    (->> messages
         (filter #(match? rule-0 %))
         count)))

;; part 2

(defn match2 [rules {message :message [rule :as stack] :stack :as state}]
  (if (empty? stack)
    (empty? message)
    (cond
      (string? rule) (if (str/starts-with? message rule)
                       (match2 rules
                               (-> state
                                   (update :stack pop)
                                   (update :message subs (count rule))))
                       false)
      (int? rule) (match2 rules
                          (-> state
                              (update :stack pop)
                              (update :stack conj (rules rule))))
      :else (let [[type & sub-rules] rule]
              (case type
                :or (some? (some #(match2 rules (-> state
                                                    (update :stack pop)
                                                    (update :stack conj %)))
                                 sub-rules))
                :cat (if (empty? sub-rules)
                       (match2 rules (update state :stack pop))
                       (match2 rules
                               (-> state
                                   (update :stack pop)
                                   (update :stack conj (concat [:cat] (rest sub-rules)))
                                   (update :stack conj (first sub-rules))))))))))

(defn match2? [rules rule message]
  (match2 rules {:message message :stack (list rule)}))

(defpart part2 [{:keys [rules messages]}]
  (let [updates-rules (-> rules
                          (assoc 8 [:or 42 [:cat 42 8]])
                          (assoc 11 [:or [:cat 42 31] [:cat 42 11 31]]))]
    (->> messages
         (filter #(match2? updates-rules 0 %))
         count)))

;; tests

(deftest match?-test
  (testing "Literal string"
    (are [rule message] (match? rule message)
      "a" "a"
      "ab" "ab")
    (are [rule message] (not (match? rule message))
      "a" "b"
      "ab" "a"
      "ab" "ac"))
  (testing "Alternative"
    (are [rule message] (match? rule message)
      [:or "a" "b"] "a"
      [:or "a" "b"] "b"
      [:or "a" "b" "c"] "a"
      [:or "a" "b" "c"] "b"
      [:or "a" "b" "c"] "c")
    (are [rule message ] (not (match? rule message))
      "a" "ab"
      "b" "a"
      [:or "a" "b"] "c"))
  (testing "Concatenation"
    (are [rule message] (match? rule message)
      [:cat "a"] "a"
      [:cat "a" "b"] "ab"
      [:cat "ab" "b" "cd"] "abbcd")
    (are [rule message] (not (match? rule message))
      [:cat "a"] "b"
      [:cat "a" "b"] "ac"))
  (testing "Mixing all kinds of rules"
    (are [rule messages] (every? #(match? rule %) messages)
      [:or [:cat "a"]] ["a"]
      [:cat "a" [:or "b" "c"]] ["ab" "ac"]
      [:or "a" [:cat "b" "c"]] ["a" "bc"]
      [:cat [:or "a" "b"] [:or "1" "2"]] ["a1" "a2" "b1" "b2"])
    (are [rule messages] (not-any? #(match? rule %) messages)
      [:cat "a" [:or "b" "c"]] ["abz" "acz" "a" "b" "c"]
      [:or "a" [:cat "b" "c"]] ["ab" "abc" "ac" "bc_"]
      [:cat [:or "a" "b"] [:or "1" "2"]] ["1a" "2a" "1a" "2a" "a" "b" "1" "2"])))

(deftest expand-rule-test
  (are [rules expected] (= expected (expand-rule rules 0))
    {0 "a"}
    "a"
    {0 [:or 1 2]
     1 "a"
     2 "b"}
    [:or "a" "b"]
    {0 [:cat 1 2]
     1 [:or "a" "b"]
     2 [:or "1" "2"]}
    [:cat [:or "a" "b"] [:or "1" "2"]]))

(deftest part1-test
  (test-with-file part1 1 2))


(deftest match2-test  
  (are [rules rule message ] (not (match2? rules rule message))
    {} [:cat [:or "a" "b"] "c"] "a" 
    ))

(deftest match2-test  
  (are [rules rule message] (match2? rules rule message)
    {} "a" "a" 
    {} [:or "a" "b"] "a" 
    {} [:or "a" "b"] "b" 
    {} [:cat "a" "b"] "ab" 
    {} [:cat [:or "a" "b"] "c"] "ac"
    {} [:cat [:or "a" "b"] "c"] "bc"
    {} [:cat [:or "a" "b"] [:or "1" "2"]] "a1"
    {} [:cat [:or "a" "b"] [:or "1" "2"]] "a2"
    {} [:cat [:or "a" "b"] [:or "1" "2"]] "b1"
    {} [:cat [:or "a" "b"] [:or "1" "2"]] "b2"
    {1 [:or "a" "b"], 2 [:or "1" "2"]} [:cat 1 2] "a1"
    {1 [:or "a" "b"], 2 [:or "1" "2"]} [:cat 1 2] "a2"
    {1 [:or "a" "b"], 2 [:or "1" "2"]} [:cat 1 2] "b1"
    {1 [:or "a" "b"], 2 [:or "1" "2"]} [:cat 1 2] "b2"
    {0 [:or 1 [:cat 1 0]], 1 "a" } 0 "a"
    {0 [:or 1 [:cat 1 0]], 1 "a" } 0 "aa"
    {0 [:or 1 [:cat 1 0]], 1 "a" } 0 "aaaaa"
    {0 [:or 1 [:cat 1 0]], 1 "a" } [:cat "^" 0 "$"] "^aaaaa$"
    {0 [:or [:cat 1 2] [:cat 1 0 2]], 1 "(", 2 ")" } 0 "()"
    {0 [:or [:cat 1 2] [:cat 1 0 2]], 1 "(", 2 ")" } 0 "(())"
    )
  (are [rules rule message] (not (match2? rules rule message))
    {} "a" "b" 
    {} "ab" "a"
    {} [:or "a" "b"] "c" 
    {} [:cat [:or "a" "b"] "c"] "a" 
    {} [:cat [:or "a" "b"] [:or "1" "2"]] "a"
    {} [:cat [:or "a" "b"] [:or "1" "2"]] "2"
    {} [:cat [:or "a" "b"] [:or "1" "2"]] "a2a"
    {0 [:or 1 [:cat 1 0]], 1 "a" } 0 ""
    {0 [:or [:cat 1 2] [:cat 1 0 2]], 1 "(", 2 ")" } 0 "(()"
    {0 [:or [:cat 1 2] [:cat 1 0 2]], 1 "(", 2 ")" } 0 "()()"
    {0 [:or [:cat 1 2] [:cat 1 0 2]], 1 "(", 2 ")" } 0 "))(("
    )
  )

(deftest match2-test
  (testing "Literal string"
    (are [rules message expected] (= expected (match2 rules 0 message))
      {0 "a"} "a" "a"
      {0 "a"} "ab" "a"
      {0 [:or 1 [:cat 1 0]], 1 "a"} "ab" "a")

))

(deftest part2-test
  (test-with-file part2 2 12))
