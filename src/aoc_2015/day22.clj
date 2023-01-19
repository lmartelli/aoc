(ns aoc-2015.day22
  (:require
   [aoc.core :refer :all]
   [aoc-2015.rpg :refer :all]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (puzzle-input-rpg-properties stream))

(def init-player {:hit-points 50, :armor 0, :mana 500, :spent 0})

;; part 1

(defn apply-healing [character {:keys [heal]}]
  (update character :hit-points + heal))

(defn add-armor [character {:keys [armor]}]
  (update character :armor + armor))

(defn remove-armor [character {:keys [armor]}]
  (update character :armor - armor))

(defn recharge-mana [character {:keys [mana]}]
  (update character :mana + mana))

(defmacro defhandler
  "Defines a function with an implicit 1st param `{:keys [registers last-freq]}`"
  [& body]
  (concat (list 'fn (vector 'this 'state)) body))

(def spells
  {:magic-missile
   {:name "magic missile" :cost 53, :damage 4,
    :cast (defhandler
            (update state :boss apply-damage this))}
   :drain
   {:name "drain" :cost 73, :damage 2 :heal 2
    :cast (defhandler
            (-> state
                (update :boss apply-damage this)
                (update :player apply-healing this)))}
   :shield
   {:name "shield" :cost 113, :timer 6, :armor 7
    :cast (defhandler
            (update state :player add-armor this))
    :expire (defhandler
              (update state :player remove-armor this))}
   :poison
   {:name "poison" :cost 173, :timer 6, :damage 3
    :turn (defhandler
            (update state :boss apply-damage this))}
   :recharge
   {:name "recharge" :cost 229, :timer 5, :mana 101
    :turn (defhandler
            (update state :player recharge-mana this))}
   })

(defn spell-event
  ([event] (fn [state spell] (spell-event state event spell)))
  ([state event spell]
   (if-let [even-handler (get-in spells [spell event])]
     (even-handler (spells spell) state)
     state)))

(defn spells-event [state event spells]
  (reduce (spell-event event) state spells))

(defn update-timers [{:keys [active-spells] :as state}]
  (let [aged-spells (update-vals active-spells dec)]
    (-> state
        (spells-event :expire (keys (filter (comp zero? val) aged-spells)))
        (assoc :active-spells (filter-vals pos? aged-spells)))))

(defn game-finished? [state]
  (cond
     (dead? (state :player)) ##Inf
     (dead? (state :boss)) (get-in state [:player :spent])
     :else nil))

(defn add-active-spell [state spell]
  (if-let [timer (get-in spells [spell :timer])]
    (assoc-in state [:active-spells spell] timer)
    state))

(defn spend [player amount]
  (-> player
      (update :mana - amount)
      (update :spent + amount)))

(defn cast-spell [state spell]
  (let [cost (get-in spells [spell :cost])]
    (-> state
        (spell-event :cast spell)
        (add-active-spell spell)
        (update :player spend cost))))

(defn available-spells [{{:keys [mana]} :player active-spells :active-spells}]
  (->> spells
       (filter
         (fn [[spell {cost :cost}]]
           (and (<= cost mana)
                (not (contains? active-spells spell)))))
       (map first)))

(defn pre-turn [state]
  (-> state
      update-timers
      (spells-event :turn (keys (state :active-spells)))))

(declare player-turn)

(defn boss-turn [{boss :boss, player :player, :as state} current-optimum]
  (let [state (pre-turn state)]
    (if-let [score (or (when (>= (player :spent) current-optimum) ##Inf) (game-finished? state))]
      score
      (player-turn
        (update state :player apply-damage boss)
        current-optimum))))

(defn player-turn [state current-optimum]
  (let [state (pre-turn state)
        available-spells (available-spells state)]
    (if-let [score (or (game-finished? state) (when (empty? available-spells) ##Inf))]
      score
      (reduce
        (fn [current-optimum spell]
          (min (boss-turn (cast-spell state spell) current-optimum) current-optimum))
        current-optimum
        available-spells))))

(defpart part1 [input]
  (player-turn {:player init-player :boss input :active-spells {}} ##Inf))

;; part 2

(defn player-turn2 [state current-optimum]
  (let [state (-> state pre-turn (update-in [:player :hit-points] dec))
        available-spells (available-spells state)]
    (if-let [score (or (game-finished? state) (when (empty? available-spells) ##Inf))]
      score
      (reduce
        (fn [current-optimum spell]
          (min (boss-turn (cast-spell state spell) current-optimum) current-optimum))
        current-optimum
        available-spells))))

(defpart part2 [input]
  (with-redefs [player-turn player-turn2]
    (part1 input)))

;; tests

(deftest spell-event-test
  (are [spell event state expected] (= expected (spell-event state event spell))
    :magic-missile :cast
    {:boss {:hit-points 9}}
    {:boss {:hit-points 5}}
    :drain :cast
    {:boss {:hit-points 9} :player {:hit-points 3}}
    {:boss {:hit-points 7} :player {:hit-points 5}}
    :shield :cast
    {:player {:hit-points 3 :armor 2}}
    {:player {:hit-points 3 :armor 9}}
    :shield :expire
    {:player {:hit-points 3 :armor 9}}
    {:player {:hit-points 3 :armor 2}}
    :poison :turn
    {:boss {:hit-points 7}}
    {:boss {:hit-points 4}}
    :recharge :turn 
    {:player {:mana 35}}
    {:player {:mana 136}})
  (are [spell event state] (= state (spell-event state event spell))
    :magic-missile :unhandled-event {:boss {:hit-points 9}}))

(deftest available-spells-test
  (are [mana active-spells expected] (= expected (set (available-spells {:player {:mana mana} :active-spells active-spells})))
    500 {} #{:magic-missile :drain :shield :poison :recharge}
    229 {} #{:magic-missile :drain :shield :poison :recharge}
    228 {} #{:magic-missile :drain :shield :poison}
    172 {} #{:magic-missile :drain :shield}
    112 {} #{:magic-missile :drain}
    72 {} #{:magic-missile}
    52 {} #{}
    500 #{:magic-missile :drain :shield :poison :recharge} #{}
    500 #{:magic-missile :drain :shield :poison} #{:recharge}
    500 #{:magic-missile :drain :shield} #{:recharge :poison}
    500 #{:magic-missile :drain} #{:recharge :shield :poison}
    500 #{:magic-missile} #{:recharge :drain :shield :poison}))

(deftest update-timers-test
  (are [state expected] (= expected (update-timers state))
    {:active-spells {:recharge 1, :shield 2, :poison 3} :player {:armor 7}}
    {:active-spells {:shield 1, :poison 2} :player {:armor 7}}

    {:active-spells {:shield 1, :poison 2} :player {:armor 7}}
    {:active-spells {:poison 1} :player {:armor 0}}))

(deftest cast-spell-test
  (are [state spell expected] (= expected (cast-spell state spell))
    {:active-spells {} :player {:mana 55 :spent 23} :boss {:hit-points 5}}
    :magic-missile
    {:active-spells {} :player {:mana 2 :spent 76} :boss {:hit-points 1}}

    {:active-spells {} :player {:armor 0 :mana 113 :spent 0}}
    :shield
    {:active-spells {:shield 6} :player {:armor 7 :mana 0 :spent 113}}))
