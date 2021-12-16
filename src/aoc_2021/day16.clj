(ns aoc-2021.day16
  (:require
   [aoc.core :refer :all]
   [clojure.pprint :refer [cl-format]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-string stream))

;; part 1

(defn bit-seq [hex-chars]
  (->> (map #(parse-int (str %) 16) hex-chars)
       (mapcat #(cl-format nil "~4,'0',B" %))
       digit-seq))

(defn bits-to-int [bits]
  (parse-binary (apply str bits)))

(defn read-bits [n bit-seq]
  [(bits-to-int (take n bit-seq))
   (drop n bit-seq)])

(defn read-raw-bits [n bit-seq]
  (split-at n bit-seq))

(defn read-bit-groups [sizes bit-seq]
  (reduce
    (fn [[values bit-seq] size]
      (let [[value more] (read-bits size bit-seq)]
        [(conj values value)more]))
    [[] bit-seq]
    sizes))

(defn eval-packet [f bit-seq]
  (letfn [(eval-literal [version bit-seq]
            (loop [literal 0
                   bit-seq bit-seq]
              (let [[[prefix bits] bit-seq] (read-bit-groups [1 4] bit-seq)
                    literal (+ (* literal 16) bits)]
                (if (= 0 prefix)
                  [((f :literal) version literal) bit-seq]
                  (recur literal bit-seq)))))
          (eval-operator [version type [length-type-id & more]]
            (case length-type-id
              0 (let [[length more] (read-bits 15 more)
                      [operands-bits more] (read-raw-bits length more)
                      operands (eval-bits operands-bits)]
                  [((f :operator) version type operands) more])
              1 (let [[nb-packets more] (read-bits 11 more)
                      [operands more] (eval-packets nb-packets more)]
                  [((f :operator) version type operands) more])))
          (eval-packets [n bit-seq]
            (-> (iterate eval-and-push-packet [[] bit-seq])
                (nth n)))
          (eval-bits [bit-seq]
            (->> (iterate eval-and-push-packet [[] bit-seq])
                 (find-first (fn [[stack more]] (empty? more)))
                 first))
          (eval-and-push-packet [[stack bit-seq]]
            (let [[value bit-seq] (eval-packet f bit-seq)]
              [(conj stack value) bit-seq]))]
    (let [[[version type] bit-seq] (read-bit-groups [3 3] bit-seq)]
      (case type
        4 (eval-literal version bit-seq)
        (eval-operator version type bit-seq)))))

(defpart part1 [input]
  (->> input
       bit-seq
       (eval-packet
         {:literal (fn [version value]
                     version)
          :operator (fn [version type operands]
                      (reduce + version operands))})
       first))

;; part 2

(defn bool-to-int [b]
  (if b 1 0))

(def operators
  {0 +
   1 *
   2 min
   3 max
   5 (comp bool-to-int >)
   6 (comp bool-to-int <)
   7 (comp bool-to-int =)})

(defpart part2 [input]
  (->> input
       bit-seq
       (eval-packet
         {:literal (fn [version value]
                     value)
          :operator (fn [version type operands]
                      (apply (operators type) operands))})
       first))

;; tests

(deftest read-bit-groups-test
  (are [sizes bits numbers remaining-bits]
      (= [numbers (digit-seq remaining-bits)]
         (read-bit-groups sizes (digit-seq bits)))
    [] "1001011" [] "1001011"
    [4] "1001011" [9] "011"
    [4 1 3] "10101011" [10 1 3] ""))

(deftest part1-test
  (are [input expected] (= expected (part1 input))
    "D2FE28" 6
    "8A004A801A8002F478" 16
    "620080001611562C8802118E34" 12
    "C0015000016115A2E0802F182340" 23
    "A0016C880162017C3686B18A3D4780" 31))

(deftest part2-test
  (are [input expected] (= expected (part2 input))
    "C200B40A82" 3
    "04005AC33890" 54
    "880086C3E88112" 7
    "CE00C43D881120" 9
    "D8005AC2A8F0" 1
    "F600BC2D8F" 0
    "9C005AC2F8F0" 0
    "9C0141080250320F1802104A08" 1))
