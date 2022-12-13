(ns aoc.core
  (:require
   [clojure.test :refer [deftest is are]]
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
  (if (str/starts-with? s "~")
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

(defn parse-aoc-ns-name [ns-name]
  (if-let [[_ year day] (re-matches #"aoc-(\d+)\.day0*(\d+)" ns-name)]
    [year day]
    (throw (Exception.
            (str "Invalid AOC namespace. Must be like \"aoc-<year>.day<day>\": " ns-name)))))

(defn puzzle-input-uri [year day]
  (str "https://adventofcode.com/" year "/day/" day "/input"))

(defn format-day [day]
  (str (when (-> day count (= 1)) "0") day))

(defn puzzle-input-filename [year day]
  (let [day (format-day day)]
    (str aoc-dir "/puzzle-input-" year "-" day ".txt")))

(defn download-puzzle-input [year day]
  (spit (puzzle-input-filename year day)
        (-> (puzzle-input-uri year day)
            (http/get {:cookies {"session" {:path "/" :value (session)}}})
            :body)))

;; puzzle input parsing

(defn puzzle-input-stream
  "Gets an input stream for the puzzle of the current namespace"
  [ns]
  (let [[year day] (parse-aoc-ns-name (str ns))
        filename (puzzle-input-filename year day)]
    (when-not (.exists (io/file filename))
      (download-puzzle-input year day))
    (io/reader filename)))

(defn *test-input*
  "Gets an input stream for the test data of puzzle of the current namespace
  (from file test/aoc_<year>/day<day>[_<suffix>].input)"
  ([ns] (*test-input* nil ns))
  ([suffix ns]
   (let [[year day] (parse-aoc-ns-name (str ns))]
     (io/reader
      (str
       "test/aoc_" year
       "/day" (format-day day)
       (if (nil? suffix) "" (str "_" suffix))
       ".input")))))

(defmacro test-input
  ([] `(*test-input* ~*ns*))
  ([suffix] `(*test-input* ~suffix ~*ns*)))

(defmacro test-data
  ([] `(~'puzzle-input (test-input)))
  ([suffix] `(~'puzzle-input (test-input ~suffix))))

(defmacro defparttest [name part expected]
  `(deftest ~name (is (= ~expected (~part (test-input))))))

(defmacro part-test
  ([part expected]
   `(is (= ~expected (~part (~'puzzle-input (test-input))))))
  ([part suffix expected]
   `(is (= ~expected (~part (~'puzzle-input (test-input ~suffix)))))))

(defmacro def-part-test [part expected]
  `(deftest ~(symbol (str part "-test"))
     (is (= ~expected (~part (~'puzzle-input (test-input)))))))

(defmacro part-tests [part expectations]
  `(are [input-num expected] (= expected (~part (~'puzzle-input (test-input input-num))))
     ~@expectations))

(defmacro parse-input-string [input]
  `(~'puzzle-input (io/input-stream (.getBytes ~input))))

(defn puzzle-input-string
  "Concatenates all the lines of the puzzle input into one string."
  ([stream] (puzzle-input-string stream identity))
  ([stream xf]
   (->> (slurp stream)
        str/split-lines
        (apply str)
        xf)))

(defn re-parse-lines [regex f lines]
  (map
   #(apply f (rest (or (re-matches regex %) (throw (Exception. (str "Regex " regex " does not match: " %))))))
   lines))

(defn split-lines [regex f lines]
  (map
   #(apply f (str/split % regex))
   lines))

(defn puzzle-input-lines [stream]
  (line-seq stream))

(defn puzzle-input-parse-lines
  "Parse all lines and then applies an optional transform on the resulting collection."
  ([stream parse] (puzzle-input-parse-lines stream parse identity))
  ([stream parse xf] (xf (mapv parse (puzzle-input-lines stream)))))

(defn puzzle-input-split-lines
  ([stream regex xf-line xf-lines]
   (puzzle-input-parse-lines stream (comp xf-line #(str/split % regex)) xf-lines))
  ([stream regex xf-line]
   (puzzle-input-split-lines stream regex xf-line identity))
  ([stream regex]
   (puzzle-input-split-lines stream regex identity)))

(defn parse-int
  ([s] (Long/parseLong s))
  ([s radix] (Long/parseLong s radix)))

(defn parse-binary [s] (parse-int s 2))

(defn bits-to-int
  "Converts a sequence of bits to an integer"
  [bits]
  (parse-binary (apply str bits)))

(defn parse-int-array
  ([input] (parse-int-array input ","))
  ([input sep]
   (mapv
     #(Long/parseLong %)
     (str/split input (re-pattern sep)))))

(defn puzzle-input-int-array [stream]
  (parse-int-array (puzzle-input-string stream)))

(defn split-input [str regex xf]
  (->> (str/split str regex)
       (map xf)))

(defn puzzle-input-split
  ([stream regex xf]
   (split-input (puzzle-input-string stream) regex xf))
  ([stream regex]
   (puzzle-input-split stream regex identity)))

;; TODO: do it lazily ?
(defn split-seq
  "Splits a sequence around items that match pred."
  [pred seq]
  (loop [splitted []
         current []
         remaining seq]
    (cond
      (empty? remaining) (conj splitted current)
      (pred (first remaining)) (recur (conj splitted current) [] (rest remaining))
      :else (recur splitted (conj current (first remaining)) (rest remaining)))))

;; misc

(defn letter? [^Character c]
  (Character/isLetter c))

(defn digit [^Character c]
  (Character/digit c 10))

(defn digit-seq
  "Parses s as a sequence of digits.
  \"123456\" will return (1 2 3 4 5 6)."
  [s]
  (map digit s))

(defn digit-vec
  "Parses s as a vector of digits.
  \"123456\" will return [1 2 3 4 5 6]."
  [s]
  (mapv digit s))

(defn remove-nil [& colls]
  (apply map #(remove nil? %) colls))

(defn read-all [channel]
  (loop [v []]
    (if-let [val (poll! channel)]
      (recur (conj v val))
      v)))

(defmacro defpart [name args & body]
  `(def ~name
     (fn
       ([] (~name (~'puzzle-input (puzzle-input-stream *ns*))))
       (~args ~@body))))

(defn array-2d-to-map
  "Converts a 2D array (a seq of seqs) to a map [x y] -> value.
  `pred` applies a filter to keep only some values."
  ([rows] (array-2d-to-map identity rows))
  ([pred rows] (array-2d-to-map pred identity rows))
  ([pred value-xf rows]
   (into
     {}
     (filter (fn [[coord val]] (pred val)))
     (mapcat
       (fn [y row]
         (map-indexed (fn [x val] [[x y] (value-xf val)]) row))
       (range) rows))))

(defn lines-to-matrix [lines value-xf]
  (mapv #(mapv value-xf %) lines))

(defn init-matrix [x y value]
  (vec (repeat y (vec (repeat x value)))))

(defn matrix-vals [m]
  (apply concat m))

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

(defn multimap
  ([entries] (multimap entries nil))
  ([entries empty-coll]
   (reduce
     (fn [m [key value]]
       (update m key (fnil conj empty-coll) value))
     {}
     entries)))

(defn positions [col pred]
  (keep-indexed
   (fn [index item]
     (when (pred item) index))
   col))

(defn first-position [col pred]
  (first (positions col pred)))

(defn range-inc
  ([to] (range-inc 0 to))
  ([from to] (range from (inc to))))

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

(defn contains [x]
  (fn [s] (contains? s x)))

(defn map-vals
  [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn map-keys
  [f m] (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn filter-vals
  [pred m] (into {} (filter (fn [[k v]] (pred v)) m)))

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

(defn manatthan-dist
  ([a b] (manatthan-dist (sub a b)))
  ([p] (->> p
            (map abs)
            (reduce +))))

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

(defn index-of [^java.util.List v value]
  (let [index (.indexOf v value)]
    (if (= -1 index)
      nil
      index)))

(defn merge-lines
  ([separator empty-value merge-fn lines]
   (reduce
     (fn [acc line]
       (if (= separator line)
         (conj acc empty-value)
         (conj (rest acc) (merge-fn (first acc) line))))
     (list empty-value)
     lines))
  ([separator empty-value merge-fn] (fn [lines] (merge-lines separator empty-value merge-fn lines))))

(defn nil-if-empty [x]
  (if (empty? x) nil x))

(defn min-max [first & more]
  (loop [m    first
         M    first
         coll more]
      (if (empty? coll)
        [m M]
        (let [[curr & more] coll]
          (recur (min m curr) (max M curr) more)))))

(defn display-grid [grid]
  (dorun (map #(println (apply str %)) grid)))

(defn display-grid-map
  "`m` is map whose keys are [x y] coordinates"
  ([m] (display-grid-map m identity))
  ([m value-xf]
   (let [[x-min x-max] (apply min-max (map first (keys m)))
         [y-min y-max] (apply min-max (map second (keys m)))]
     (-> (map
           (fn [y]
             (map
               (fn [x]
                 (if-let [value (m [x y])] (value-xf value) \space))
              (range-inc x-min x-max)))
          (range-inc y-min y-max))
        display-grid)
    m)))

(defn vec-min-max [first & more]
  (loop [res (map vector first first)
         [cur & more] more]
    (if (nil? cur)
      res
      (recur
        (map (fn [v [m M]] [(min v m) (max v M)]) cur res)
        more))))

(defn byte-to-quad-bits [b]
  (let [int-value (bit-and b 0xff)]
    [(quot int-value 16) (mod int-value 16)]))

(defn bytes-to-quadbits [bytes]
  (mapcat byte-to-quad-bits bytes))

(defn bytes-to-hex [^bytes bytes]
  (->> bytes
       (BigInteger. 1)
       (format (str "%0" (* 2 (count bytes))  "x"))))

(defn signum [n]
  (cond
    (zero? n) 0
    (pos? n) 1
    :else -1))

(defn transpose [rows]
  (apply map list rows))

(defn truncate [n v]
  (subvec v 0 n))
