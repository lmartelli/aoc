(ns aoc-2019.day14
  (:require
   [clojure.string :as str :refer [split]]
   [aoc.core :refer :all]
   [clojure.algo.generic.functor :refer [fmap]]
   [clojure.math.numeric-tower :refer [ceil]]
   [clojure.test :refer :all]))

(defn parse-element [s]
  (let [[_ qty element] (re-matches #"(\d+) *(\w+)" s)]
    [element (parse-int qty)]))

(defn parse-elements [s]
  (into {} (map parse-element (split s #" *, *"))))

(defn parse-reaction [line]
  (let [[inputs output] (split line #" *=> *")
        input-elements (parse-elements inputs)
        output-element (parse-element output)]
    {:in input-elements :out output-element}))

(def puzzle-input (puzzle-input-parse-lines parse-reaction))

;; part 1

(defn map-reactions-byoutput-elt [reactions]
  (reduce
   (fn [reactions r] (assoc reactions (get-in r [:out 0]) r))
   {}
   reactions))

(def reactions (map-reactions-byoutput-elt puzzle-input))

(defn substitute [[elt qty] reactions]
  (let [r (reactions elt)]
    (if (and r (pos? qty))
      (let [out-qty (get-in r [:out 1])
            mult (-> (/ qty out-qty) Math/ceil long)
            unused (- (* out-qty mult) qty)]
        ;;(println "Using " mult "×" r)
        (merge
         (fmap #(* % mult) (r :in))
         (when (not= qty (* out-qty mult))
           {elt (- unused)})))
      {elt qty})))

(def memo-substitute (memoize substitute))

(defn remove-zeros [elements]
  (into {} (filter (fn [[k v]] (not (zero? v))) elements)))

(defn substitute-all [elements reactions]
  (->> elements
       (reduce
        #(merge-with + %1 (substitute %2 reactions))
        {})
       remove-zeros))

(defn mine [qty input]
  (let [reactions (map-reactions-byoutput-elt input)]
    (loop [elements {"FUEL" qty}]
      ;;(println "elements:" elements)
      (if (and (= '("ORE") (for [[k v] elements :when (pos? v)] k)))
        (elements "ORE")
        (recur (substitute-all elements reactions))))))


(defn part1 [input] (mine 1 input))

;; part 2

(defn part2 [input]
  (let [ore-qty 1000000000000
        mine (fn [qty] (mine qty input))]
    (loop [cur (quot ore-qty (mine 1))]
      (let [mine-cur (mine cur)]
        (println "cur" cur "(mine cur)" mine-cur)
        (cond (and (<= mine-cur ore-qty)
                   (> (mine (inc cur)) ore-qty))
              cur
              (< mine-cur ore-qty)
              (recur (-> (* (/ ore-qty mine-cur) cur) ceil long))
              :else
              (recur (dec cur))
      )))))

;; tests

(deftest parse-element-test
  (is (= ["TRUC" 13] (parse-element "13  TRUC"))))

(deftest parse-reaction-test
  (is (= {:in {"MACHIN" 2, "BIDULE" 3} :out ["TRUC" 1]}
         (parse-reaction "2 MACHIN , 3 BIDULE => 1 TRUC"))))

(deftest map-reactions-byoutput-elt-test
  (is (= {"Z" {:in {"A" 2 "B" 3} :out ["Z" 5]}, "Y" {:whatever "blablabla" :out ["Y" 42]}}
         (map-reactions-byoutput-elt [{:in {"A" 2 "B" 3} :out ["Z" 5]}
                                      {:whatever "blablabla" :out ["Y" 42]}]))))

(defn index-reactions [lines]
  (map-reactions-byoutput-elt
   (map parse-reaction lines)))

(deftest substitute-test
  (are [reactions elt qty expected] (= expected (substitute [elt qty] (index-reactions reactions)))
    ["2 A, 3 B => 5 C"] "C" 5 {"A" 2 "B" 3}
    ["2 A, 3 B => 5 C"] "C" 4 {"A" 2 "B" 3 "C" -1}
    ["2 A, 3 B => 5 C"] "C" 6 {"A" 4 "B" 6 "C" -4}
    ["2 A, 3 B => 5 C"] "C" 10 {"A" 4 "B" 6} 
    ["2 A, 3 B => 5 C"] "C" 11 {"A" 6 "B" 9 "C" -4}
   ))

(deftest substitute-all-test
  (are [reactions elts expected] (= expected (substitute-all elts (index-reactions reactions)))
    ["2 A, 3 B => 5 C"] { "C" 5 } {"A" 2 "B" 3}
    ["2 A, 3 B => 5 C"
     "1 A, 3 C => 1 X"] { "X" 2 } {"A" 2 "C" 6}
    ["2 A, 3 B => 5 C"
     "1 A, 3 C => 1 D"] { "C" 1 "D" 1 } {"A" 3 "C" -1 "B" 3 }
   ))

(deftest remove-zeros-test
  (are [m expected] (= expected (remove-zeros m))
    {} {}
    {:a 1 :b 2} {:a 1 :b 2}
    {:a 0 :b 2 :c 0 :d 4} {:b 2 :d 4}))

(def test-input-1 "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")


