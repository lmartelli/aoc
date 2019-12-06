;; 2019-5

(def input [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,6,23,2,6,23,27,2,27,9,31,1,5,31,35,1,35,10,39,2,39,9,43,1,5,43,47,2,47,10,51,1,51,6,55,1,5,55,59,2,6,59,63,2,63,6,67,1,5,67,71,1,71,9,75,2,75,10,79,1,79,5,83,1,10,83,87,1,5,87,91,2,13,91,95,1,95,10,99,2,99,13,103,1,103,5,107,1,107,13,111,2,111,9,115,1,6,115,119,2,119,6,123,1,123,6,127,1,127,9,131,1,6,131,135,1,135,2,139,1,139,10,0,99,2,0,14,0])

(defn run-op [opcodes ip]
  (let [target (get opcodes (+ ip 3))
        op1 (get opcodes (get opcodes (+ ip 1)))
        op2 (get opcodes (get opcodes (+ ip 2)))]
    (case (get opcodes ip)
      1 (assoc opcodes target (+ op1 op2))
      2 (assoc opcodes target (* op1 op2)))))

(defn get-digit [n pos]
  (mod (reduce quot n (repeat (dec pos) 10)) 10))

(defn param-mode [instr n]
  (get-digit instr (+ 2 n)))

(defn op-code [instr]
  (mod instr 100))

(def debug? false)

(defn debug [& args]
  (if debug?
    (apply println args)))

(defn run-op [memory ip in out]
  (let [instr (memory ip)
        op-code (op-code instr)
        imm (fn [n] (memory (+ ip n)))
        arg (fn [n]
              (let [immediate (imm n)]
                (case (param-mode instr n)
                  1 immediate
                  0 (memory immediate))))]
    (debug "instr:" instr "op-code:" op-code)
    (case op-code
      ;; + *
      (1 2) (let [op ({1 + 2 *} op-code)]
              (debug op (arg 1) (arg 2) "→" (imm 3))
              [(+ ip 4)
               (assoc memory
                      (imm 3)
                      (op
                       (arg 1)
                       (arg 2)))
               in
               out])
      ;; input
      3 [(+ ip 1)
         (assoc memory (arg 1) (first in))
         (rest in)
         out]
      ;; output
      4 [(+ ip 1)
         memory
         in
         (conj out (arg 1))]
      )
    ))

(defn run [memory in]
  (loop [memory memory
         ip 0
         in in
         out []]
    (if (= (get opcodes ip) 99)
      (get opcodes 0)
      (recur (run-op opcodes ip) (+ ip 4)))
  ))

(defn cartesian-poduct [coll]
    (for [x coll
          y coll]
      [x y]))

(defn find-noun-and-verb [expected]
  (loop [nouns-and-verbs (cartesian-poduct (range 0 100))]
    (let [[noun verb] (first nouns-and-verbs)
          result (run input noun verb)]
      (if (= expected result)
        (+ (* 100 noun) verb)
        (recur (rest nouns-and-verbs))))))
