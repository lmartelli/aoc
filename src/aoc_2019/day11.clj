(ns aoc-2019.day11
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as s2]
   [aoc.ocr :refer :all]
   [aoc-2019.day05 :refer [debug set-debug]]
   [aoc-2019.day07 :refer [terminated?]]
   [aoc-2019.day09 :refer [run-instr]]
   [clojure.test :refer :all]))

(def-input-parser [[line]]
  (parse-ints line))

;; part 1

(defn run [state halt-condition]
  (loop [state (merge {:ip 0, :base 0, :mem [], :in [], :out []} state)]
    (debug "ip:" (state :ip))
    (if (or (terminated? state) (halt-condition state))
      state
      (recur (run-instr state)))))

(defn halt-condition [state]
  (= 2 (-> state :out count)))

(defn rotate [robot instr]
  (case instr
    0 (update robot :dir s2/rotate-left)
    1 (update robot :dir s2/rotate-right)))

(defn advance [robot]
  (let [dir (robot :dir)]
    (update robot :pos s2/+ dir)))

(defn move-robot [robot instr]
  (-> robot
      (rotate instr)
      advance))

(defn run-robot [prog first-panel]
  (loop [robot {:pos [0 0] :dir [0 -1]}
         painted-panels {}
         computer-state {:ip 0 :base 0 :mem prog :in [first-panel] :out []}]
    (let [new-computer-state (run computer-state halt-condition)
          out (new-computer-state :out)]
      ;;(println "out:" out)
      (if (halt-condition new-computer-state)
        (let [new-robot (move-robot robot (second out))]
          (recur
           new-robot
           (assoc painted-panels (robot :pos) (first out))
           (assoc new-computer-state
                  :in [(or (painted-panels (new-robot :pos) 0))]
                  :out [])))
        {:robot robot, :painted-panels painted-panels}
        )
    )))

(defpart part1 [input]
  (-> (run-robot input 0)
      :painted-panels
      count))

;; part 2

(defn init-panels [width height]
  (vec (repeat height (vec (repeat width \space)))))

(defn paint-panels [painted-panels]
  (->> painted-panels
       (map-vals {0 \space, 1 \#})
       (s2/print-to-lines)))

(defpart part2 [input]
  (->> (run-robot input 1)
       :painted-panels
       (filter-vals (eq 1))
       paint-panels
       ocr))

;; tests

(deftest rotate-test
  (are [robot instr new-dir] (= (assoc robot :dir new-dir) (rotate robot instr))
    {:pos [1 2] :dir [0 1]} 0 [1 0]))

(deftest move-robot-test
  (are [robot instr expected] (= expected (move-robot robot instr))
    {:pos [0 0] :dir [0 1]} 0 {:pos [1 0] :dir [1 0]}))

(deftest init-panels-test
  (is (= [[\space \space \space] [\space \space \space]] (init-panels 3 2))))
