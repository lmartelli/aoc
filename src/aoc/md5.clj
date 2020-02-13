(ns aoc.md5)

(import java.security.MessageDigest)

(defn md5 [^String s]
  (->> s
       .getBytes
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn md5-seq [key]
  (map #(md5 (str key %)) (range)))

(defn md5-seq [key]
  (map #(md5 (str key %)) (range)))
