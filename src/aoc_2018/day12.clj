(ns aoc-2018.day12
  (:require
   [aoc.core :refer :all]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn dot-to-space [s]
  (str/replace s #"\." " "))

(defn puzzle-input [stream]
  (let [[initial-state & notes] (filter not-empty (line-seq stream))]
    {:initial-state [0 (-> (re-find #"[.#]+" initial-state) dot-to-space)]
     :notes (->> notes
                 (filter #(str/ends-with? % "#"))
                 (map #(re-find #"[.#]+" %))
                 (map dot-to-space)
                 (into #{}))}))

;; part 1

(defn next-gen [[first-pot pots] notes]
  (let [new-pots (->> (concat "    " pots "    ")
                      (partition 5 1)
                      (map #(if (notes (str/join %)) \# \space))
                      str/join)]
    [(+ (str/index-of new-pots \#)
        (- first-pot 2))
     (str/trim new-pots)]))

(defn generations [initial-state notes]
  (iterate
    #(next-gen % notes)
    initial-state))

(defn sum-pots-with-plant [[first-pot pots]]
    (->> (map (fn [pos pot] (if (= pot \#) pos 0)) (iterate inc first-pot) pots)
         (reduce +)))

(defpart part1 [{:keys [initial-state notes]}]
  (let [gen-20 (nth (generations initial-state notes) 20)]
    (sum-pots-with-plant gen-20)))

;; part 2

(defpart part2 [{:keys [initial-state notes]}]
  (let [{:keys [start-pos length start-value repeat-value]} (find-cycle-key second (generations initial-state notes))
        [first-pot-start pots] start-value
        [first-pot-repeat pots] repeat-value]
    (if (not= 1 length)
      (throw (Exception. "Can only handle cycle of length 1"))
      (let [first-pot (-> (- 50000000000 start-pos) (* (- first-pot-repeat first-pot-start)) (+ first-pot-start))]
        (sum-pots-with-plant [first-pot pots])))))

;; tests

(deftest generations-test
  (let [{:keys [initial-state notes]} (test-data)]
    (is (= [[0 "#  # #  ##      ###   ###"]
            [0 "#   #    #     #  #  #  #"]
            [0 "##  ##   ##    #  #  #  ##"]
            [-1 "# #   #  # #    #  #  #   #"]
            [0 "# #  #   # #   #  #  ##  ##"]
            [1 "#   ##   # #  #  #   #   #"]
            [1 "## # #    #   #  ##  ##  ##"]
            [0 "#  ### #   ##  #   #   #   #"]
            [0 "#    ## # # #  ##  ##  ##  ##"]
            [0 "##  #  #####    #   #   #   #"]
            [-1 "# #  #   # ##    ##  ##  ##  ##"]]
           (->> (generations initial-state notes)
                (take 11))))))

(deftest part1-test (part-test part1 325))

(deftest part2-test)
