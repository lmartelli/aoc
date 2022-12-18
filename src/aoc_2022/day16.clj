(ns aoc-2022.day16
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [clojure.math.combinatorics :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (re-seq-parse-lines
         #"[A-Z]{2}|\d+"
         (fn [valve rate & neighbours]
           (hash-map :valve (keyword valve)
                     :rate (parse-int rate)
                     :neighbours (into {} (map #(vector (keyword %) 1) neighbours)))))))

;; part 1

(defn init-rates [input]
  (reduce
    (fn [rates {:keys [valve rate]}]
      (assoc rates valve rate))
    {}
    input))

(defn init-neighbours [input]
  (mapcat (fn [{:keys [valve neighbours]}] (map  neighbours)) input)
  (reduce
    (fn [m {:keys [valve neighbours]}]
      (assoc m valve neighbours))
    {}
    input))

(defn next-states [{:keys [current-valve rates] :as state}, neighbours]
  (concat (when (pos? (get-in rates [current-valve]))
            [(assoc-in state [:rates current-valve] 0)])
          (map #(assoc state :current-valve %) (neighbours current-valve))))

(defn pass-over-zero-rate-valve [neighbours zero-rate-valve]
  (reduce
    (fn [neighbours [n1 n2]]
      (let [joined-dist (+ (get-in neighbours [n1 zero-rate-valve])
                           (get-in neighbours [n2 zero-rate-valve]))]
        (-> neighbours
            (update-in [n1 n2] #(or %1 %2) joined-dist)
            (update-in [n2 n1] #(or %1 %2) joined-dist))))
    neighbours
    (combinations (keys (neighbours zero-rate-valve)) 2)))

(defn remove-zero-rate-valve [neighbours zero-rate-valve init-neighbours]
  (let [zero-rate-valve-neighbours (into (set (keys (init-neighbours zero-rate-valve)))
                                         (keys (neighbours zero-rate-valve)))]
    (reduce
      (fn [neighbours valve]
        (if (neighbours valve)
          (update neighbours valve dissoc zero-rate-valve)
          neighbours))
      (if (= :AA zero-rate-valve)
        neighbours
        (dissoc neighbours zero-rate-valve))
      zero-rate-valve-neighbours)))

(defn remove-zero-rate-neighbours [rates init-neighbours]
  (let [zero-rate-valves (->> rates
                              (filter (fn [[valve rate]] (zero? rate)))
                              (map key))
        pass-over-zero-rate-valves (fn [neighbours]
                                     (reduce pass-over-zero-rate-valve neighbours zero-rate-valves))
        remove-zero-rate-valves (fn [neighbours]
                                  (reduce #(remove-zero-rate-valve %1 %2 init-neighbours) neighbours zero-rate-valves))]    
    (-> init-neighbours
        pass-over-zero-rate-valves
        remove-zero-rate-valves)))

(defn floyd-warshall [distances rates]
  (let [nodes (keys distances)]
    (reduce
      (fn [distances node]
        (reduce
          (fn [distances [from to]]
            (update-in distances [from to]
                       #(min (or % ##Inf)
                             (+ (get-in distances [from node] ##Inf)
                                (get-in distances [node to] ##Inf)))))
          distances
          (for [from nodes, to nodes
                :when (and (not= from node)
                           (not= to node)
                           (not= from to)
                           (pos? (rates to)))]
            [from to])))
      distances
      nodes)
    )
  )

(defn find-max-pressure [distances rates]
  (letfn [(dfs [valves-to-visit
                current-valve
                time-left
                released-pressure]
            (if (or (empty? valves-to-visit) (zero? time-left))
              released-pressure
              (->> valves-to-visit
                   (map #(let [next-time-left (- time-left (int (get-in distances [current-valve %])) 1)]
                           (dfs
                             (disj valves-to-visit %)
                             %
                             next-time-left
                             (+ released-pressure (* next-time-left (int (rates %)))))))
                   (apply max))))]
    (dfs (set (->> (filter (comp pos? val) rates) (map key)))
         :AA
         30
         0)))

(defpart part1 [input]
  (let [rates (init-rates input)
        neighbours (init-neighbours input)
        distances (-> (remove-zero-rate-neighbours rates neighbours)
                      (floyd-warshall rates))]
    (find-max-pressure distances rates)))

;; part 2

(defpart part2 [input]
  nil)

;; tests

(deftest next-states-test
  (let [neighbours {:a [:b :c]
                    :b [:a]
                    :c [:a]}]
    (is (= #{{:current-valve :b, :rates {:a 0}}
             {:current-valve :c, :rates {:a 0}}}
           (set (next-states {:current-valve :a :rates {:a 0}}
                             neighbours)))
        "do not open zero rate valve")
  (are [state expected] (= expected (set (next-states state neighbours)))
    {:current-valve :a :rates {:a 2}}
    #{{:current-valve :a :rates {:a 0}}
      {:current-valve :b :rates {:a 2}}
      {:current-valve :c :rates {:a 2}}}
    )))

(deftest part1-test)

(deftest part2-test)
