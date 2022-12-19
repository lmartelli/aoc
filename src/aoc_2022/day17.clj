(ns aoc-2022.day17
  (:require
   [aoc.core :refer :all]
   [aoc.space-2d :as space-2d]
   [aoc.algo :as algo]
   [clojure.string :as str]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (puzzle-input-string stream)
       (map {\< [-1 0], \> [1 0]})))

;; part 1

(def rock-shapes
  [[[0 0] [1 0] [2 0] [3 0]]
   [[1 0] [0 1] [1 1] [2 1] [1 2]]
   [[0 0] [1 0] [2 0] [2 1] [2 2]]
   [[0 0] [0 1] [0 2] [0 3]]
   [[0 0] [0 1] [1 0] [1 1]]])

(defn collision? [chamber rock]
  (some chamber rock))

(defn try-move [rock move chamber]
  (let [moved (map #(space-2d/+ % move) rock)]
    (if (collision? chamber moved)
      rock
      moved)))

(defn init-chamber []
  #{[space-2d/segment-points [[-1 2] [-1 -1] [7 -1] [7 2]]]})

(defn create-rock [{:keys [height rocks] :as state}]
  (->  state
       (assoc :rock (map #(space-2d/+ % [2 (+ height 3)]) (first rocks))
              :rocks (rest rocks))
       (update :chamber into (space-2d/segment-points [[-1 height] [-1 (+ height 7)]]))
       (update :chamber into (space-2d/segment-points [[7 height] [7 (+ height 7)]]))))

(defn rock-step [rock chamber jet-dir]
  (reduce
    #(try-move %1 %2 chamber)
    rock
    [jet-dir [0 -1]]))

(defn moved-down? [[[xa ya] & more-a] [[xb yb] & more-b]]
  (not= ya yb))

(defn place-rock [state rock]
  (-> state
      (update :height max (inc (apply max (map second rock))))
      (update :chamber into rock)
      (update :nb-rock inc)
      create-rock))

(defn step [{:keys [chamber height rock rocks] :as state} jet-dir]
  (let [new-rock (rock-step rock chamber jet-dir)]
    (if (moved-down? rock new-rock)
      (assoc state :rock new-rock)
      (place-rock state new-rock))))

(defn simulate [jet-dirs]
  (->>
    (reductions
      step
      (create-rock
        {:chamber (set (space-2d/segment-points [[-1 2] [-1 -1] [7 -1] [7 2]]))
         :height 0
         :nb-rock 0
         :rock nil
         :rocks (cycle rock-shapes)})
      (cycle jet-dirs))
    (partition-by :nb-rock)
    (map first)))

(defn show-state [{:keys [rock chamber]}]
  (let [y-max (apply max (map second rock))]
    (space-2d/print
      (-> (space-2d/draw-points {} \# chamber)
          (space-2d/draw-points \@ rock)
          (space-2d/draw-points \┃ (space-2d/segment-points [[-1 y-max] [-1 0]]))
          (space-2d/draw-points \┃ (space-2d/segment-points [[7 y-max] [7 0]]))
          (space-2d/draw-points \━ (space-2d/segment-points [[0 -1] [6 -1]]))
          (space-2d/draw-points \┗ [[-1 -1]])
          (space-2d/draw-points \┛ [ [7 -1]])
          )
      (range -1 8)
      (range y-max -2 -1))))

(defpart part1 [input]
  (-> (simulate input)
      (nth 2022)
      :height))

;; part 2

(defn get-y-min [points]
  (apply min (map second points)))

(defn get-y-max [points]
  (apply max (map second points)))

(defn relative-to-y-min [points]
  (map #(space-2d/- % [0 (get-y-min points)]) points))

(defn top-config [{:keys [chamber height rock]}]
  (let [y-max (get-y-max rock)
        chamber-with-rock (into chamber rock)]
    (-> (algo/explore :start [0 (+ height 2)]
                  :neighbours (fn [p] (->> (space-2d/direct-neighbours p)
                                           (filter (fn [[x y]] (and (<= 0 x 7) (<= y y-max))))))
                  :neighbour-allowed? (not (chamber-with-rock neighbour-pos))
                  :stop? (empty? last-visited))
        :visited
        relative-to-y-min
        set)))

(defn show-top [points]
  (let [y-max (get-y-max points)]
    (println "---------")
    (space-2d/print
      (space-2d/draw-points {} \# points)
      (range -1 8)
      (range y-max (dec 0) -1))))

(defn find-cycle [seq]
  (loop [visited {}
         pos 0
         [current & more] seq]
    (if-let [start (visited current)]
      {:start-pos start
       :start-value current
       :length (- pos start)}
      (recur (assoc visited current pos)
             (inc pos)
             more))))

(defn find-cycle-key [f seq]
  (loop [visited {}
         current-pos 0
         [[current-val current-key] & more] (map #(vector % (f %)) seq)]
    (if-let [[start-pos start-val] (visited current-key)]
      {:start-pos start-pos
       :start-value start-val
       :end-value current-val
       :length (- current-pos start-pos)}
      (recur (assoc visited current-key [current-pos current-val])
             (inc current-pos)
             more))))

(defn find-top-config-cycle [input]
  (->> (simulate input)
       (take-nth 5 #_(count input))
       (find-cycle-key top-config)))

(defn height-at-nb-rocks [input n]
  (-> (simulate (test-data))
      (nth n)
      :height))

(defpart part2 [input]
  (let [target 1000000000000
        {start :start-value, end :end-value} (find-top-config-cycle input)
        start-pos (start :nb-rock)
        cycle-length (- (end :nb-rock) start-pos)
        cycle-elevation (- (end :height) (start :height))
        nb-last-rocks (mod (- target start-pos) cycle-length)]
    (println "found cycle: " {:start-pos start-pos
                              :length cycle-length
                              :elevation cycle-elevation})
    (+ (start :height)
       (* (quot (- target start-pos) cycle-length)
          cycle-elevation)
       (apply - (map #(height-at-nb-rocks input %) [(+ start-pos nb-last-rocks) start-pos])))))

;; tests

(deftest part1-test (part-test part1 3068))

(deftest part2-test (part-test part2 1514285714288))
