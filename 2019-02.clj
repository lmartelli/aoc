;; 2019-2
(defn parse-input [input]
  (vec
   (map (fn [str] (Integer/parseInt str))
        (clojure.string/split input #","))))

(def input (parse-input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,19,6,23,2,6,23,27,2,27,9,31,1,5,31,35,1,35,10,39,2,39,9,43,1,5,43,47,2,47,10,51,1,51,6,55,1,5,55,59,2,6,59,63,2,63,6,67,1,5,67,71,1,71,9,75,2,75,10,79,1,79,5,83,1,10,83,87,1,5,87,91,2,13,91,95,1,95,10,99,2,99,13,103,1,103,5,107,1,107,13,111,2,111,9,115,1,6,115,119,2,119,6,123,1,123,6,127,1,127,9,131,1,6,131,135,1,135,2,139,1,139,10,0,99,2,0,14,0"))

(defn run-op [opcodes ip]
  (let [target (get opcodes (+ ip 3))
        op1 (get opcodes (get opcodes (+ ip 1)))
        op2 (get opcodes (get opcodes (+ ip 2)))]
    (case (get opcodes ip)
      1 (assoc opcodes target (+ op1 op2))
      2 (assoc opcodes target (* op1 op2)))))

(defn run-op [memory ip]
  (let [operator (case (get memory ip)
                   1 +
                   2 *)
        operand-value (fn [a] (get memory (get memory (+ ip a))))]
    (assoc memory
           (get memory (+ ip 3))
           (operator
            (operand-value 1)
            (operand-value 2)))))

(defn run [input noun verb]
  (loop [opcodes (assoc (parse-input input)
                        1 noun
                        2 verb)
         ip 0]
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
