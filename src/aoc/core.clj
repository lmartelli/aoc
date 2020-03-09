(ns aoc.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.math.numeric-tower :refer [abs sqrt]]
   [clj-http.client :as http]
   [clojure.core.async :as async :refer [poll!]]
   [clojure.pprint :refer [cl-format]]))

;; puzzle input downloading

(defn system-get-property [p]
  (System/getProperty p))

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (system-get-property "user.home"))
    s))

(def aoc-dir (expand-home "~/.aoc"))

(defn session []
  (try (slurp (str aoc-dir "/session"))
       (catch java.io.FileNotFoundException e nil)))

(defn replace-chars
  ([s m] (apply str (replace m s)))
  ([s from-chars to-chars]
   (replace-chars s (zipmap from-chars to-chars))))

(defn ns-name [ns]
  (str (clojure.core/ns-name ns)))

(defn parse-aoc-ns-name [ns-name]
  (if-let [[_ year day] (re-matches #"aoc-(\d+)\.day0*(\d+)" ns-name)]
    [year day]
    (throw (Exception.
            (str "Invalid AOC namespace. Must be like \"aoc-<year>.day<day>\": " ns-name)))))

(defn puzzle-input-uri [year day]
  (str "https://adventofcode.com/" year "/day/" day "/input"))

(defn puzzle-input-filename [year day]
  (let [day (str (when (-> day count (= 1)) "0") day)]
    (str aoc-dir "/puzzle-input-"year "-" day ".txt")))

(defn download-puzzle-input [year day]
  (spit (puzzle-input-filename year day)
        (-> (puzzle-input-uri year day)
            (http/get {:cookies {"session" {:path "/" :value (session)}}})
            :body)))

;; puzzle input parsing

(defn $puzzle-input-stream [ns]
  (let [[year day] (parse-aoc-ns-name (ns-name ns))
        filename (puzzle-input-filename year day)]
    (when-not (.exists (io/file filename))
      (download-puzzle-input year day))
    (io/reader filename)))

(defn $puzzle-input-string [ns]
  (->> ($puzzle-input-stream ns)
       slurp
       str/split-lines
       (apply str)))

(defmacro puzzle-input-string
  ([] `(puzzle-input-string identity))
  ([xf] `(def ~'puzzle-input (~xf ($puzzle-input-string *ns*)))))

(defn $puzzle-input-lines [ns]
  (->> ($puzzle-input-stream ns)
       io/reader
       line-seq))

(defmacro puzzle-input-parse-stream [f]
  `(def ~'puzzle-input (~f ($puzzle-input-stream *ns*))))

(defmacro puzzle-input-lines
  ([] `(puzzle-input-lines identity))
  ([xf] `(def ~'puzzle-input (~xf ($puzzle-input-lines *ns*)))))

(defn $puzzle-input-parse-lines
  ([lines parse] ($puzzle-input-parse-lines lines parse identity))
  ([lines parse xf] (xf (mapv parse lines))))

(defmacro puzzle-input-parse-lines
  ([f] `(def ~'puzzle-input ($puzzle-input-parse-lines ($puzzle-input-lines *ns*) ~f)))
  ([f xf] `(def ~'puzzle-input ($puzzle-input-parse-lines ($puzzle-input-lines *ns*) ~f ~xf))))

(defmacro puzzle-input-split-lines
  ([regex xf-line xf-lines]
   `(puzzle-input-parse-lines (comp ~xf-line #(str/split % ~regex)) ~xf-lines))
  ([regex xf-line]
   `(puzzle-input-split-lines ~regex ~xf-line identity))
  ([regex]
   `(puzzle-input-split-lines ~regex identity)))

(defn parse-int [s] (Long/parseLong s))

(defn parse-int-array [input]
   (mapv
    #(Long/parseLong %)
    (str/split input #",")))

(defmacro puzzle-input-int-array []
  `(def ~'puzzle-input (parse-int-array ($puzzle-input-string *ns*))))

(defmacro puzzle-input-parse [f]
  `(def ~'puzzle-input (~f ($puzzle-input-string *ns*))))

(defn split-input [str regex xf]
  (->> (str/split str regex)
       (map xf)))

(defmacro puzzle-input-split
  ([regex xf]
   `(def ~'puzzle-input (split-input ($puzzle-input-string *ns*) ~regex ~xf)))
  ([regex]
   `(def ~'puzzle-input (split-input ($puzzle-input-string *ns*) ~regex identity))))

;; misc

(defn letter? [c]
  (Character/isLetter c))

(defn digit [c]
  (Character/digit c 10))

(defn digit-seq
  "Parses s as a sequence of digits.
  \"123456\" will return (1 2 3 4 5 6)."
  [s]
  (map digit s))

(defn remove-nil [& colls]
  (apply map #(remove nil? %) colls))

(defn read-all [channel]
  (loop [v []]
    (if-let [val (poll! channel)]
      (recur (conj v val))
      v))
  )

(defmacro defpart [name args body]
  `(def ~name
     (fn
       ([] (~name ~'puzzle-input))
       (~args ~body))))

(defn array-2d-to-map
  "`pred` applies a filter to keep only some values."
  ([rows] (array-2d-to-map identity rows))
  ([pred rows]
   (into
    {}
    (filter (fn [[[x y] val]] (pred val)))
     (mapcat
      (fn [row y]
        (mapv (fn [val x] [[x y] val]) row (range)))
      rows (range)))))

(defn array-2d-to-set
  "Returns a set of coordinates of `on-char` values."
  [on-char rows]
  (->> rows
       (array-2d-to-map #{on-char})
       keys
       set))

(defn init-matrix [x y value]
  (vec (repeat y (vec (repeat x value)))))

(defn grep [regex seq]
  (filter #(re-find regex %) seq))

(defn find-first [f seq]
  (first (filter f seq)))

(defn find-last [f seq]
  (last (take-while f seq)))

(defn combinations-with-sum
  "Combinations of n positive integers constrained by a sum"
  [n sum]
  (if (= 1 n)
    (list [sum])
    (mapcat
     (fn [x]
       (map #(conj % x) (combinations-with-sum (dec n) (- sum x))))
     (range (inc sum)))))

(defn get-wrap [v index]
  (get v (mod index (count v))))

(defn multimap [entries]
  (reduce
   (fn [m [key value]]
     (update m key conj value))
   {}
   entries))

(defn positions [col pred]
  (keep-indexed
   (fn [index item]
     (when (pred item) index))
   col))

(defn first-position [col pred]
  (first (positions col pred)))

(defn range-inc [from to]
  (range from (inc to)))

(defn expand-bag [bag]
  (mapcat (fn [[k v]] (repeat v k)) bag))

(defn update!
  ([m k f]
   (assoc! m k (f (get m k))))
  ([m k f x]
   (assoc! m k (f (get m k) x)))
  ([m k f x y]
   (assoc! m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc! m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc! m k (apply f (get m k) (concat [x y z] more)))))

(defn group-by-pred [pred coll]
  [(filter pred coll)
   (filter (comp not pred) coll)])

(defn max-val [m]
  (val (apply max-key val m)))

(defn hex [ints]
  (apply str (map #(format "%02x" %) ints)))

(defn bin [ints]
  (apply str (map #(cl-format nil "~8,'0',B" %) ints)))

(defn range-width [coll]
  (if (empty? coll)
    nil
    (- (apply max coll) (apply min coll))))

(defn eq [x]
  (fn [y] (= x y)))

(defn square [n]
  (* n n ))

(defn multiple?
  "Is n a multiple of m ?"
  [n m]
  (zero? (mod n m)))

;; vector

(defn add "Vector addition"
  [a & rest]
  (apply mapv + a rest))

(defn sub "Vector subtraction"
  [a & rest]
  (apply mapv - a rest))

(defn mult "Vector multiplication by a number: v × n"
  [v n]
  (mapv #(* % n) v))

(defn div "Vector division by a number: v × 1/n"
  [v n]
  (mapv #(/ % n) v))

(defn center [v]
  (mult v (/ 1 2)))

(defn prod "Scalar product of 2 vectors" [u v]
  (reduce + (map * u v)))

(defn norm [v]
  (sqrt (reduce + (map square v))))

(defn cos [v u]
  (/ (prod u v) (* (norm u) (norm v))))

(defn- transform-relative [p origin tx]
  (-> p
      (sub origin)
      tx
      (add origin)))

(defn rotate-left "Y axis points down"
  ([[x y]] [y (- x)])
  ([p center] (transform-relative p center rotate-left)))

(defn rotate-right "Y axis points down"
  ([[x y]] [(- y)  x])
  ([p center] (transform-relative p center rotate-right)))

(defn flip-vert "Y axis points down"
  ([[x y]] [(- x) y])
  ([p [cx cy]] (transform-relative p [cx 0] flip-vert)))

(defn flip-horiz "Y axis points down"
  ([[x y]] [x (- y)])
  ([p [cx cy]] (transform-relative p [0 cy] flip-horiz)))

(defn move [pos dir dist]
  (add pos (mult dir dist)))

(defn manatthan-dist [p]
  (->> p
       (map abs)
       (reduce +)))

(defn remove-index [v index]
  (vec
   (concat
    (subvec v 0 index)
    (subvec v (inc index)))))

(defn insert-at [v pos value]
  (-> (subvec v 0 pos)
      (conj value)
      (into (subvec v pos))))

(defn shift-right
  ([v] (shift-right v 1))
  ([v n]
   (let [size (count v)]
     (if (zero? size)
       v
       (let [pos (- size (mod n size))]
         (vec (concat (subvec v pos) (subvec v 0 pos))))))))

(defn shift-left
  ([v] (shift-left v 1))
  ([v n] (shift-right v (- n))))

(defn swap [v i1 i2]
  (assoc v
         i1 (v i2)
         i2 (v i1)))

(defn index-of [v value]
  (let [index (.indexOf v value)]
    (if (= -1 index)
      nil
      index)))
