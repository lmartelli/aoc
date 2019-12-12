(ns aoc.day7)

(require
 '[aoc.day5 :refer [parse-string parse-input set-debug]]
 '[clojure.pprint :refer [pprint]])

(def puzzle-input-7 (parse-input "2019-07.txt"))


(defn cur-instr [state]
  ((state :mem) (state :ip)))

(defn terminated? [state] (= (op-code (cur-instr state)) 99))

(defn run-instr [state]
  (let [instr (cur-instr state)
        {ip :ip, memory :mem, in :in, out :out} state
        op-code (op-code instr)
        imm (fn [n] (memory (+ ip n)))
        arg (fn [n]
              (let [immediate (imm n)]
                (case (param-mode instr n)
                  1 immediate
                  0 (memory immediate))))]
    (debug "instr:" instr "op-code:" op-code)
    (case op-code
      ;; +
      1 (do (debug "+" (arg 1) (arg 2) "→" (imm 3))
            (assoc state
                   :ip (+ ip 4)
                   :mem (assoc memory
                               (imm 3) (+' (arg 1) (arg 2)))))
      ;; *
      2 (do (debug "*" (arg 1) (arg 2) "→" (imm 3))
            (assoc state
                   :ip (+ ip 4)
                   :mem (assoc memory
                               (imm 3) (*' (arg 1) (arg 2)))))
      ;; input → [1]
      3 (let [input-value (first in)]
          (debug "input" input-value "→" (imm 1))
          (assoc state
                 :ip (+ ip 2)
                 :mem (assoc memory (imm 1) (first in))
                 :in (rest in)))
      ;; output ← [1]
      4 (assoc state
               :ip (+ ip 2)
               :out (conj out (arg 1)))
      ;; jump-if-true
      5 (do (debug "jump-if-true" (arg 1))          
            (assoc state
                   :ip (if (not= 0 (arg 1)) (arg 2) (+ ip 3))))
      ;; jump-if-false
      6 (do (debug "jump-if-false" (arg 1))
            (assoc state
                   :ip (if (= 0 (arg 1)) (arg 2) (+ ip 3))))
      ;; less than
      7 (do (debug "less than" (arg 1) "<" (arg 2) "→" (imm 3))
            (assoc state
                   :ip (+ ip 4)
                   :mem (assoc memory (imm 3) (if (< (arg 1) (arg 2)) 1 0))))
      ;; equals
      8 (do (debug "equals" (arg 1) "=" (arg 2) "→" (imm 3))
            (assoc state
                   :ip (+ ip 4)
                   :mem (assoc memory (imm 3) (if (= (arg 1) (arg 2)) 1 0))))
      )))

(defn run [state]
  (loop [state state]
    (debug "ip:" (state :ip))
    (if (or (not-empty (state :out))
            (terminated? state))
      state
      (recur (run-instr state)))))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn run-amp [state]
  (debug "in" (state :in))
  (let [new-state (run state)
        out (new-state :out)]
      (debug "out" out)
      (first out)))

(defn run-amps [amp-states]
  (let [nb-amps (count amp-states)]
    (loop [n 0
           amps amp-states]
      ;(println nb-amps "amps:" amps)
      (let [amp (get amps n)]
        (if (nil? amp)
          amps
          (do
          (println "Running Amp" n "in:" (amp :in) "out:" (amp :out))
          (let [new-amp (run amp)
                ret-value (new-amp :out)]
            (println "      → Amp" n "in:" (new-amp :in) "out:" (new-amp :out))
            ;(println "Amp" n "=" new-amp)
            (recur
             (inc n)
             (-> amps
                 (assoc n (assoc new-amp :out []))
                 (update-in [(mod (inc n) nb-amps) :in] concat ret-value))))))))))
  
(defn init-amps [mem phases]
  (update-in 
   (vec (map #(hash-map :ip 0 :mem mem :in [%] :out [])  phases))
   [0 :in] conj 0))

(defn loop-value [amp-states]
  (-> amp-states
      first
      :in
      first))

(defn find-max-thrust [mem]
  (apply max
         (map #(loop-value (run-amps (init-amps mem %)))
              (permutations (range 5)))))

;; part 2

(defn run-amps-loop [mem phases]
  (println "phases" phases)
  (loop [amps (init-amps mem phases)
         thrust nil]
;;    (println (count amps) "amps")
    ;;    (println amps)
    (let [new-thrust (loop-value amps)]
      (println "thrust" thrust "→" new-thrust)
      (if (= thrust new-thrust)
        (do (println "→" (loop-value amps))
            amps)
        (recur (run-amps amps) new-thrust)))))

(defn find-max-thrust-loop [mem]
  (apply max
         (map #(loop-value (run-amps-loop mem %))
              (permutations (range 5 10)))))
