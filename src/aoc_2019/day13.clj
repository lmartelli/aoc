(ns aoc-2019.day13
  (:require
   [aoc.core :refer :all]
   [aoc-2019.day05 :refer [debug set-debug op-code param-mode]]
   [aoc-2019.day07 :refer [terminated? cur-instr]]
   [clojure.core.async :as async :refer [>!! poll! close! chan]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (mapcat parse-ints)
       (into [])))

;; part 1

(def tile-types { 0 :empty, 1 :wall, 2 :block, 3 :paddle, 4 :ball})

(defn run-instr [state in out]
  (let [instr (cur-instr state)
        {ip :ip, base :base, memory :mem} state
        op-code (op-code instr)
        mem-get (fn [addr] (or (get memory addr) 0))
        mem-set (fn [addr value]
                  (try
                    (assoc memory addr value)
                    (catch IndexOutOfBoundsException e
                      (assoc (-> memory (concat (repeat (- addr (count memory)) 0)) vec) addr value))))
        arg-out (fn [n]
                 (let [value (mem-get (+ ip n))]
                   (case (param-mode instr n)
                     0 value
                     2 (+ base value)
                     )))
        arg-in (fn [n]
                 (let [value (mem-get (+ ip n))]
                   (case (param-mode instr n)
                     0 (mem-get value)
                     1 value
                     2 (mem-get (+ base value))
                     )))]
    (debug "instr:" instr "op-code:" op-code)
    (merge
     state
     (case op-code
       ;; +
       1 (do (debug "+" (arg-in 1) (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (+' (arg-in 1) (arg-in 2)))})
       ;; *
       2 (do (debug "*" (arg-in 1) (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (*' (arg-in 1) (arg-in 2)))})
       ;; input → [1]
       3 (let [input-value (in)]
           (debug "input" input-value "→" (arg-out 1))
           {:ip (+ ip 2)
            :mem (mem-set (arg-out 1) input-value) })
       ;; output ← [1]
       4 (do (debug "output" (arg-in 1))
             (>!! out (arg-in 1))
             { :ip (+ ip 2) })
       ;; jump-if-true
       5 (do (debug "jump-if-true" (arg-in 1))
             {:ip (if (not= 0 (arg-in 1)) (arg-in 2) (+ ip 3))})
       ;; jump-if-false
       6 (do (debug "jump-if-false" (arg-in 1))
             {:ip (if (= 0 (arg-in 1)) (arg-in 2) (+ ip 3))})
       ;; less than
       7 (do (debug "less than" (arg-in 1) "<" (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (if (< (arg-in 1) (arg-in 2)) 1 0))})
       ;; equals
       8 (do (debug "equals" (arg-in 1) "=" (arg-in 2) "→" (arg-out 3))
             {:ip (+ ip 4)
              :mem (mem-set (arg-out 3) (if (= (arg-in 1) (arg-in 2)) 1 0))})
       ;; adjust relative base
       9 (do (debug "adjust relative base" (arg-in 1))
             {:ip (+ ip 2)
              :base (+ base (arg-in 1))})
       ))))

(defn run [state in out]
  (loop [state (merge {:ip 0, :base 0, :mem []} state)]
    (debug "ip:" (state :ip))
    (if (terminated? state)
      state
      (recur (run-instr state in out)))))

(defpart part1 [input]
  (let [out (chan 10000 (comp (partition-all 3)
                              (map last)
                              (map tile-types)))]
    (run {:mem input} nil out)
    (->> (read-all out)
         frequencies
         :block)))

;; part 2

(def adapt-screen-cmds
  (comp
    (partition-all 3)
    (map (fn [[x y n]]
           (if (= [-1 0] [x y])
             n
             [x y (tile-types n)])))))

(defpart part2 [input]
  (let [out (chan 10000 adapt-screen-cmds)
        score (atom 0)
        paddle-x (atom nil)
        ball-x (atom nil)
        handle-out (fn [cmd]
                     (if (integer? cmd)
                       (reset! score cmd)
                       (let [[x y tile] cmd]
                         (case tile
                           :ball (reset! ball-x x)
                           :paddle (reset! paddle-x x)
                           nil))))
        play (fn []
               (run! handle-out (read-all out))
               (let [move (cond
                            (< @paddle-x @ball-x) 1
                            (> @paddle-x @ball-x) -1
                            :else 0)]
                 move))]
    (run {:mem (assoc input 0 2)} play out)
    (close! out)
    (run! handle-out (read-all out))
    @score))

;; tests

(deftest adapt-screen-cmds-test
  (is (= [[1 2 :empty] [3 4 :wall] [5 6 :block] [7 8 :paddle] [9 10 :ball]]
         (into [] adapt-screen-cmds [1 2 0, 3 4 1, 5 6 2, 7 8 3, 9 10 4]))))
