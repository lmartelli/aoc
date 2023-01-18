(ns aoc-2016.day11
  (:require
   [aoc.core :refer :all]
   [aoc.algo :as algo]
   [clojure.math.combinatorics :refer [combinations]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer :all]))

(defn parse-items [items-str]
  (if (= "nothing relevant" items-str)
    {}
    (->> (reduce
          (fn [items item-str]
            (re-parse item-str
                      #"a ([a-z]+)(?:-compatible)? (microchip|generator)"
                      #(update items (keyword %2) (fnil conj #{}) (keyword %1))))
          {}
          (str/split items-str #"( *,| *and)+ *")))))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (re-parse-lines #"contains (.*)\." parse-items)
       (into [])))

;; part 1

(defn items-valid? [{generators :generator chips :microchip}]
  (or (empty? generators)
      (every? generators chips)))

(defn to-items [items-seq]
  (reduce
   (fn [items [type element]]
     (update items type (fnil conj #{}) element))
   {}
   items-seq))

(defn to-seq [items]
  (concat (map #(vector :microchip %) (items :microchip))
          (map #(vector :generator %) (items :generator))))

(defn subsets [items & {min :min max :max}]
  (mapcat (fn [n] (combinations items n))
          (range-inc (or min 0) (or max (count items)))))

(defn normalize-items [items]
  (reduce
   (fn [items empty-key]
     (dissoc items empty-key))
   items
   (->> (keys items)
        (filter #(empty? (items %))))))

(defn remove-items [items removed-items]
  (-> (merge-with set/difference items removed-items)
      normalize-items))

(defn loadable-seq [items]
  (->> (subsets (to-seq items) :min 1 :max 2)
       (map to-items)
       (filter items-valid?)
       (filter #(items-valid? (remove-items items %)))))

(defn add-items [items-a items-b]
  (merge-with into items-a items-b))

(defn next-states [{:keys [floors floor] :as state}]
  (for [loaded (loadable-seq (floors floor))
        dest-floor (->> [(inc floor) (dec floor)] (filter #(< -1 % (count floors))))
        :let [dest-items (add-items (floors dest-floor) loaded)]
        :when (items-valid? dest-items)]
    {:floors (-> floors
                 (assoc dest-floor dest-items)
                 (update floor remove-items loaded))
     :floor dest-floor}
  ))

(defn init-state [floors]
  {:floors floors :floor 0})

(defn final-state [floors]
  {:floors (conj (into [] (repeat (dec (count floors)) {}))
                 (reduce add-items {} floors))
   :floor (dec (count floors))})

(defpart part1 [floors]
  (-> (let [end-state (final-state floors)]
        (algo/explore :start (init-state floors)
                      :neighbours next-states
                      :stop? (last-visited end-state)))
      :nb-steps))

;; part 2

(defpart part2 [floors]
  (part1 (update floors 0 add-items {:microchip #{:elerium :dilithium} :generator #{:elerium :dilithium}})))

;; tests

(deftest normalize-items-test
  (testing "Noop if contains no empty for a kind"
    (are [items] (= items (normalize-items items))
      {:x #{:A}}
      {:y #{:B}}
      {:x #{:A} :y #{:B}}
      ))
  (testing "Removes keys with empty set"
    (are [items normalized] (= normalized (normalize-items items))
      {:x #{}} {}
      {:x #{} :y #{}} {}
      {:x #{:A} :y #{}} {:x #{:A}})))

(deftest remove-items-test
  (testing "removing all items results in an empty floor"
    (are [items] (= {} (remove-items items items))
      {:microchip #{:A}}
      {:microchip #{:A :B}}
      {:microchip #{:A :B} :generator #{:A}}))
  (testing "leaves non removed items"
    (are [items-a items-b expected] (= expected (remove-items items-a items-b))
      {:microchip #{:A :B}}
      {:microchip #{:B}}
      {:microchip #{:A}}

      {:microchip #{:A} :generator #{:A}}
      {:microchip #{:A}}
      {:generator #{:A}}
)))

(deftest up-to-2-compatible-items-can-be-loaded-in-the-elevator
  (is (empty? (loadable-seq {})) "No items to load")
  (testing "only one item to load"
    (are [items expected] (= expected (set (loadable-seq items)))
      {:microchip #{:A}} #{{:microchip #{:A}}}
      {:generator #{:A}} #{{:generator #{:A}}}))
  (testing "Can load up to 2 microchip"
    (are [items expected] (= expected (set (loadable-seq items)))
      {:microchip #{:A :B}} #{{:microchip #{:A}}
                              {:microchip #{:B}}
                              {:microchip #{:A :B}}}))
  (testing "Can load up a microchip and a compatible generator"
    (are [items expected] (= expected (set (loadable-seq items)))
      {:microchip #{:A} :generator #{:A}} #{{:microchip #{:A}}
                                            {:generator #{:A}}
                                            {:microchip #{:A} :generator #{:A}}}))
  (testing "Cannot load microchip with incompatible generator"
    (are [items expected] (= expected (set (loadable-seq items)))
      {:microchip #{:A} :generator #{:A :B}} #{{:microchip #{:A}}
                                               {:microchip #{:A} :generator #{:A}}
                                               {:generator #{:B}}
                                               {:generator #{:A :B}}}
      ))
  )

(deftest parse-input-test
  (is (= [{:microchip #{:lithium :hydrogen}}
          {:generator #{:hydrogen}}
          {:generator #{:lithium}}
          {}]
         (test-data))))

(deftest items-valid?-test
  (testing "microchips can be together without any generator"
    (are [items] (items-valid? items)
      {}
      {:microchip #{:A}}
      {:microchip #{:A} :generator #{}}
      {:microchip #{:A :B}  :generator #{}}
      {:microchip #{:A :B :C}  :generator #{}}))
  (testing "microchips can be together with a compatible generator"
    (are [items] (items-valid? items)  
      {:microchip #{:A} :generator #{:A}}
      ))
  (testing "microchips can be together with a incompatible generator if also with a compatible generator"
    (are [items] (items-valid? items)  
      {:microchip #{:A} :generator #{:A :B}}
      {:microchip #{:A :B} :generator #{:A :B}}
      ))
  (testing "generators can be together without any microchip"
    (are [items] (items-valid? items)
      {:generator #{:A}}
      {:microchip #{} :generator #{:A}}
      {:generator #{:A :B}}
      {:microchip #{} :generator #{:A :B}}))  
  (testing "a microchip cannot be together with an incompatible generator"
    (are [items] (not (items-valid? items))
      {:microchip #{:A} :generator #{:B}}
      {:microchip #{:A :B} :generator #{:B}}))
  )

(deftest next-states-test
  (is (= [{:floors [{:microchip #{:lithium}}
                    {:microchip #{:hydrogen} :generator #{:hydrogen}}
                    {:generator #{:lithium}}
                    {}]
           :floor 1}]
       (next-states (init-state (test-data))))))

(deftest part1-test (part-test part1 11))

(deftest part2-test)
