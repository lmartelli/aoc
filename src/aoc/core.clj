(ns aoc.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clj-http.client :as http]
   [clojure.core.async :as async :refer [poll!]]))

(defn system-get-property [p]
  (System/getProperty p))

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (system-get-property "user.home"))
    s))

(def aoc-dir (expand-home "~/.aoc"))

(def session
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
            (http/get {:cookies {"session" {:path "/" :value session}}})
            :body)))

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

(defmacro puzzle-input-string []
  '(def ~'puzzle-input ($puzzle-input-string *ns*)))

(defn $puzzle-input-lines [ns]
  (->> ($puzzle-input-stream ns)
       io/reader
       line-seq))

(defmacro puzzle-input-parse-stream [f]
  `(def ~'puzzle-input (~f ($puzzle-input-stream *ns*))))

(defmacro puzzle-input-lines []
  `(def ~'puzzle-input ($puzzle-input-lines *ns*)))

(defmacro puzzle-input-parse-lines [f]
  `(def ~'puzzle-input (map ~f ($puzzle-input-lines *ns*))))

(defn parse-int [s] (Long/parseLong s))

(defn parse-int-array [input]
  (vec
   (map
    #(Long/parseLong %)
    (str/split input #","))))

(defmacro puzzle-input-int-array []
  `(def ~'puzzle-input (parse-int-array ($puzzle-input-string *ns*))))

(defn digit-seq
  "Parses s as a sequence of digits.
  \"123456\" will return (1 2 3 4 5 6.)"
  [s]
  (map #(Character/digit % 10) s))

(defmacro puzzle-input-parse [f]
  `(def ~'puzzle-input (~f ($puzzle-input-string *ns*))))

(defn remove-nil [& colls]
  (apply map #(remove nil? %) colls))

(defn add "Vector addition"
  [a & rest]
  (vec (apply map + a rest)))

(defn sub "Vector subtraction"
  [a & rest]
  (vec (apply map - a rest)))

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
