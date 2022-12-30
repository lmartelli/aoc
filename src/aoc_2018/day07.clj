(ns aoc-2018.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn successors [instructions]
  (reduce
    (fn [successors [pred succ]]
      (update successors pred conj succ))
    {}
    instructions))

(defn predecessors [instructions]
  (reduce
    (fn [preds [pred succ]]
      (update preds succ conj pred))
    {}
    instructions))

(defn tasks [instructions]
  (set (apply concat instructions)))

(defn puzzle-input [stream]
  (let [instructions (->> (line-seq stream)
                          (re-parse-lines #"Step ([A-Z]) .*step ([A-Z]).*"
                                          #(vector %1 %2)))]
    {:successors (successors instructions)
     :predecessors (predecessors instructions)
     :tasks (tasks instructions)}))

;; part 1

(defn roots [nodes successors]
  (let [all-successors (->> (map val successors)
                            (reduce #(apply conj %1 %2) #{}))]
    (remove all-successors nodes)))

(defn order-tasks [{:keys [successors tasks]}]
  (loop [tasks tasks
         successors successors
         ordered-tasks []]
    (if (empty? tasks)
      ordered-tasks
      (let [next-task (->> (roots tasks successors) sort first)]
        (recur
          (disj tasks next-task)
          (dissoc successors next-task)
          (conj ordered-tasks next-task))))))

(defpart part1 [input]
  (-> (order-tasks input)
      str/join))

;; part 2

(defn duration [fixed-cost]
  #(+ (inc fixed-cost) (- (int (first %)) (int \A))))

(defn runnable? [task done preds]
  (every? done (preds task)))

(defn completion-time [{succs :successors preds :predecessors tasks :tasks} nb-workers duration-fn]
  (loop [available-tasks (set (roots tasks succs))
         running-tasks {}
         available-workers nb-workers
         current-time 0
         finished-tasks #{}]
    #_(println "time:" current-time "available-workers:" available-workers "available-tasks:" available-tasks "running:" running-tasks)
    (if (= tasks finished-tasks)
      current-time
      (let [runnable-tasks (take available-workers (sort-by duration-fn available-tasks))]
        (if (not-empty runnable-tasks)
          (do
            #_(println "Start" runnable-tasks)
            (recur
              (apply disj available-tasks runnable-tasks)
              (into running-tasks
                    (map #(vector % (+ current-time (duration-fn %))) runnable-tasks))
              (- available-workers (count runnable-tasks))
              current-time
              finished-tasks))
          (let [[finished-task time] (first (sort-by val running-tasks))
                new-finished-tasks (conj finished-tasks finished-task)]
            #_(println finished-task "finished at" time)
            (recur
              (into available-tasks
                    (->> (succs finished-task)
                         (remove finished-tasks)
                         (filter #(runnable? % new-finished-tasks preds))))
              (dissoc running-tasks finished-task)
              (inc available-workers)
              time
              new-finished-tasks)
            ))))))

(defpart part2 [input]
  (completion-time input 5 (duration 60)))

;; tests

(deftest order-tasks-test
  (are [successors expected] (= expected (order-tasks successors))
    [["A" "B"]] "AB"
    [["A" "C"]
     ["A" "B"]] "ABC"
    [["A" "B"]
     ["B" "C"]] "ABC"
    [["A" "B"]
     ["A" "D"]
     ["B" "C"]
     ["D" "C"]] "ABDC"
    ))

(deftest part1-test (part-test part1 "CABDFE"))

(deftest completion-time-test
  (is (= 15 (completion-time (test-data) 2 (duration 0)))))
