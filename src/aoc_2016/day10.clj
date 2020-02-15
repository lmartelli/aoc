(ns aoc-2016.day10
  (:require
   [aoc.core :refer :all]
   [instaparse.core :as insta]))

(def parser
  (insta/parser
   "<R> = INIT | DISPATCH
    INIT = <'value '> INT <' goes to bot '> INT
    DISPATCH = <'bot '> INT <' gives low to '> DEST <' and high to '> DEST
    <DEST> = BOT | OUTPUT
    BOT = <'bot '> INT
    OUTPUT = <'output '> INT
    INT = #'[0-9]+'"))

(puzzle-input-parse-lines
 (fn [line]
   (->> line
        parser
        first
        (insta/transform
         {:INT parse-int
          :DISPATCH (fn [bot low high] {:cmd :dispatch, :bot bot, :low low, :high high })
          :INIT (fn [value bot] {:cmd :init, :bot bot, :value value})}))))

;; part 1

(defn add-to [m id value]
  (update m id #(into (sorted-set) (conj % value))))

(defn init-bots [rules]
  (->> rules
       (filter #(= :init (:cmd %)))
       (reduce
        (fn [bots {:keys [bot value]}]
          (add-to bots bot value))
        {})))

(defn dispatch-rules [rules]
  (->> rules
       (filter #(= :dispatch (:cmd %)))
       (map (fn [{:keys [bot low high]}]
              [bot {:low low, :high high}]))
       (into {})))

(defn update-bots-or-outputs [m [bot-id vals] {low-dest :low high-dest :high} dest-type]
  (let [[low-val high-val] (vec vals)]
    (let [operations [[low-val low-dest] [high-val high-dest]]]
      (->> (filter #(= dest-type (first (second %))) operations)
           (map (fn [[value [_ dest-id]]] [value dest-id]))
           (reduce
            (fn [m [value dest-id]] (add-to m dest-id value))
            m)))))

(defn update-bots [bots [bot-id vals :as bot] rule]
  (update-bots-or-outputs (dissoc bots bot-id) bot rule :BOT))

(defn update-outputs [outputs bot rule]
  (update-bots-or-outputs outputs bot rule :OUTPUT))

(defn bot-full? [[id chips]]
  (= (count chips) 2))

(defn dispatch [{:keys [bots outputs] :as state} rules]
  (->> (filter bot-full? bots)
       (reduce
        (fn [{:keys [bots outputs]} [bot-id chips :as bot]]
          (let [rule (rules bot-id)]
            ;;(println "apply rule" rule "on" bot)
            {:bots (update-bots bots bot rule)
             :outputs (update-outputs outputs bot rule)}))
        state)))

(defpart part1 [rules]
  (->> {:bots (init-bots rules) :outputs {}}
       (iterate #(dispatch % (dispatch-rules rules)))
       (map :bots)
       (take-while not-empty)
       (map #(filter bot-full? %))
       (apply concat)
       (filter #(= #{17 61} (val %)))
       first
       key))

;; part 2

(defpart part2 [rules]
  (->> {:bots (init-bots rules) :outputs {}}
       (iterate #(dispatch % (dispatch-rules rules)))
       (filter #(empty? (:bots %)))
       first
       :outputs
       (filter #(#{0,1,2} (key %)))
       (mapcat val)
       (apply *)))

;; tests
