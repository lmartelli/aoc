(ns aoc-2019.day11
  (:require
   [aoc.core :refer :all]
   [aoc-2019.day05 :refer [debug set-debug]]
   [aoc-2019.day07 :refer [terminated?]]
   [aoc-2019.day09 :refer [run-instr]]
   [clojure.test :refer :all]))

(puzzle-input-int-array)

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
    0 (update robot :dir rotate-left)
    1 (update robot :dir rotate-right)))

(defn advance [robot]
  (let [dir (robot :dir)]
    (update robot :pos add dir)))

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

(defn boundaries [coordinates]
  (reduce
   (fn [boundaries coord]
     (-> boundaries
         (update :left min (coord 0))
         (update :right max (coord 0))
         (update :top min (coord 1))
         (update :bottom max (coord 1))))
   {:left 0, :right 0, :top 0, :bottom 0}
   coordinates))

(defn paint-panel [panels [[x y] color]]
  (println "paint " [x y] "→" color)
  (assoc-in panels [y x]
             (case color
               0 \space
               1 \#)))

(defn display-panels [lines]
  (map #(apply str %) lines))

(defn init-panels [width height]
  (vec (repeat height (vec (repeat width \space)))))

(defn paint-panels [painted-panels]
  (let [{top :top left :left bottom :bottom right :right} (boundaries (keys painted-panels))
        origin [top left]
        width (inc (- right left))
        height (inc (- bottom top))]
    (println "origin" origin)
    (println "boundaries" top left bottom right)
    (println "dim" [width height])
    (reduce
     paint-panel
     (init-panels width height)
     (map #(update % 0 sub origin) painted-panels))
    ))

(defpart part2 [input]
  (-> (run-robot input 1)
      :painted-panels
      paint-panels
      display-panels))

;; tests

(deftest rotate-left-test
  (are [dir rotated] (= rotated (rotate-left dir))
       [0 -1] [-1 0]
       [-1 0] [0 1]
       [0 1] [1 0]
       [1 0] [0 -1]))

(deftest rotate-right-test
  (are [dir rotated] (= rotated (rotate-right dir))
       [0 -1] [1 0]
       [1 0] [0 1]
       [0 1] [-1 0]
       [-1 0] [0 -1]))

(deftest rotate-test
  (are [robot instr new-dir] (= (assoc robot :dir new-dir) (rotate robot instr))
    {:pos [1 2] :dir [0 1]} 0 [1 0]))

(deftest move-robot-test
  (are [robot instr expected] (= expected (move-robot robot instr))
    {:pos [0 0] :dir [0 1]} 0 {:pos [1 0] :dir [1 0]}))

(deftest boundaries-test
  (is (= {:left -3 :right 2 :top -5 :bottom 4}
         (boundaries [[1 2] [0 0] [-1 4] [2 -5] [-3 -3] [-3 -5]]))))

(deftest init-panels-test
  (is (= [[\space \space \space] [\space \space \space]] (init-panels 3 2))))

(deftest paint-panel-test
  (let [init (init-panels 4 3)
        = (fn [a b] (clojure.core/= a (display-panels b)))]
    (is (= ["    "
            "    "
            "    "]
           (display-panels (paint-panel init [[2 2] 0]))))
    (is (= ["#   "
            "    "
            "    "]
           (display-panels (paint-panel init [[0 0] 1]))))
    (is (= ["    "
            "  # "
            "    "]
           (display-panels (paint-panel init [[2 1] 1]))))
    ))
