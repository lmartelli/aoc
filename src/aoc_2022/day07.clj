(ns aoc-2022.day07
  (:require
   [aoc.core :refer :all]
   [clojure.string :refer [split]]
   [clojure.test :refer :all]))

(defn puzzle-input [stream]
  (->> (line-seq stream)
       (map (fn [line]
              (condp #(starts-with? %2 %1) line
                "$ cd" [:cd (subs line 5)]
                "$ ls" [:ls]
                "dir" [:dir (subs line 4)]
                (let [[size name] (split line #" ")]
                  [:file name (parse-int size)]))))))

;; part 1


(defn cd [cwd dir]
  (case dir
    "/" []
    ".." (pop cwd)
    (conj cwd dir)))

(defn dirname [dir]
  (str "/" (join "/" dir)))

(defn subpaths [path]
  (reduce
   (fn [paths name]
     (let [prev (peek paths)]
       (conj paths (conj prev name))))
   '[[]]
   path))

(defn dir-sizes [input]
  (-> (reduce
       (fn [{:keys [cwd dirs] :as state} [cmd & args]]
         (case cmd
           :cd (update state :cwd cd (first args))
           :file (reduce
                  (fn [state path]
                    (update-in state [:dirs (dirname path)] (fnil + 0) (second args)))
                  state
                  (subpaths cwd)) 
           state))
       {}
       input)
      :dirs))

(defpart part1 [input]
  (->> input
       dir-sizes
       vals
       (filter #(<= % 100000))
       (reduce +)))

;; part 2

(defpart part2 [input]
  (let [sizes (dir-sizes input)
        disk 70000000
        required 30000000
        used (sizes "/")
        free (- disk used)
        ]
    (->> (vals sizes)
         (filter #(>= (+ free %) required))
         (apply min))
    ))

;; tests

(deftest dir-sizes-test
  (are [input expected] (= expected (dir-sizes input))
    [[:cd "/"]] nil
    [[:cd "/"] [:file "a" 1]] { "/" 1}
    [[:cd "/"]
     [:file "a" 1]
     [:file "b" 2]]
    { "/" 3}
    [[:cd "/"]
     [:cd "home"]
     [:file "a" 1]]
    { "/" 1 "/home" 1}
    [[:cd "/"]
     [:cd "home"]
     [:file "a" 1]
     [:cd ".."]
     [:cd "usr"]
     [:file "b" 2]]
    { "/" 3 "/home" 1 "/usr" 2}
    ))

(deftest part1-test (part-test part1 95437))

(deftest part2-test (part-test part2 24933642))
