(ns aoc.md5)

(defn md5 [^String s]
  (->> s
       .getBytes
       (.digest (java.security.MessageDigest/getInstance "MD5"))))
