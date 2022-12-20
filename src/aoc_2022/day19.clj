(ns aoc-2022.day19
  (:require
   [aoc.core :refer :all]
   [clojure.math :refer [ceil]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map parse-ints)
       (map
         (fn [[id n1 n2 n3 n4 n5 n6]]
           {:id id
            :specs {:ore{:ore n1}
                    :clay {:ore n2}
                    :obsidian {:ore n3 :clay n4}
                    :geode {:ore n5 :obsidian n6}}}))))

;; part 1

(defn can-make-robot? [target stock blueprint]
  (and stock
       (every?
         (fn [[element required-quantity]]
           (>= (stock element 0) required-quantity))
         (blueprint target))))

(defn limiting-factor [target stock production blueprint]
  (->> (blueprint target)
       (apply max-key
              (fn [[element required-quantity]]
                (if-let [prod (production element)]
                  (/ (- required-quantity (stock element 0))
                     prod)
                  ##Inf)))
       key))

(defn forecast-time-to-target [{:keys [stock production]} target blueprint]
  (->> (blueprint target)
       (map (fn [[element required-quantity]]
                (if-let [prod (production element)]
                  (-> (/ (- required-quantity (stock element 0))
                         prod)
                      ceil
                      int)
                  ##Inf)))
       (apply max)))

(defn update-stock [state f quantities]
  (reduce
    (fn [state [element quantity]]
      (update-in state [:stock element] (fnil f 0) quantity))
    state
    quantities))

(defn build-robot [state target blueprint]
  (if (nil? target)
    state
    (-> state
        (update-stock - (blueprint target))
        (update-in [:production target] (fnil inc 0)))))

(defn update-state [state robot blueprint]
  (cond-> (update-stock state + (state :production))
    robot (build-robot robot blueprint)))

(defn decide [{:keys [stock production] :as state} blueprint]
  (if (every? zero? (vals stock))
    nil
    (loop [target :geode]
      (if (can-make-robot? target stock blueprint)
        target
        (let [new-target (limiting-factor target stock production blueprint)]
          (if (= target new-target)
            nil
            (recur new-target)))))))

(defn max-geode-heuristic [state time-left blueprint]
  (loop [time 0
         state state]
    (if (>= time time-left)
      (get-in state [:stock :geode])
      (let [robot (decide state blueprint)]
        (recur (inc time) (update-state state robot blueprint))))))

(defn buildable-robots [{:keys [stock] :as state} best blueprint]
  (let [robots (into #{nil}
                     (->> blueprint
                          (filter
                            (fn [[robot required-elements]]
                              (can-make-robot? robot stock blueprint)))
                          (map key)))]
    (if (robots :geode)
      #{:geode}
      robots)))

(defn max-geode [state time-left best blueprint]
  (if (zero? time-left)
    (get-in state [:stock :geode] 0)
    (reduce
      (fn [best robot]
        (let [candidate (max-geode (update-state state robot blueprint) (dec time-left) best blueprint)]
          (if (> candidate best)
            (debug-val candidate "new best: ")
            best)))
      best
      (buildable-robots state best blueprint)
      )))

(def initial-state {:production {:ore 1}})

(defpart part1 [input]
  )

;; part 2

(defpart part2 [input]
  nil)

;; tests

(deftest decide-test
  (testing "Geode robot can be built"
    (are [stock geode-blueprint] (= :geode (decide {:stock stock} {:geode geode-blueprint}))
      {:ore 5 :obsidian 7 } {:ore 5 :obsidian 7 }
      {:ore 6 :obsidian 7 } {:ore 5 :obsidian 7 }
      {:ore 5 :obsidian 8 } {:ore 5 :obsidian 7 }
      {:ore 6 :obsidian 8 } {:ore 5 :obsidian 7 }
      )
    )
  (testing "Nothing can be done"
    (let [blueprint (-> (test-data) first :specs)
          production {:ore 1}]
      (are [stock expected] (= expected (decide {:stock stock :production production} blueprint))
        {} nil
        {:ore 1} nil)))
  )

(deftest limiting-factor-test
  (testing "choose the element that reach required quantity the latest"
    (let [target :A
          blueprint {:A {:B 10 :C 3}}]
      (are [stock production expected] (= expected (limiting-factor target stock production blueprint))
        {}     {:B 2 :C 1} :B
        {}     {:B 3 :C 1} :B
        {}     {:B 4 :C 1} :C
        {:B 6} {:B 2 :C 1} :C
        {}     {:B 1}      :C
        {}     {     :C 1} :B
        ))
    ))

(deftest max-geode-heuristic-test
  (are [blueprint expected] (= expected (max-geode-heuristic initial-state 24 blueprint))
    (-> (test-data) first :specs) 8
    (-> (test-data) second :specs) 10
    ))

(deftest max-geode-test
  (are [heuristic blueprint expected] (= expected (max-geode initial-state 24 heuristic blueprint))
    8 (-> (test-data) first :specs) 9
    10 (-> (test-data) second :specs) 12
    ))

(deftest part1-test (part-test part1 33))

(deftest part2-test)
