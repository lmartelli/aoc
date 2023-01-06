(ns aoc-2016.assembunny
  (:require
   [aoc.cpu :refer :all]))

(def instruction-set
  {:cpy (instr [src dest] (set-reg dest src))
   :inc (instr [r] (update-reg r inc))
   :dec (instr [r] (update-reg r dec))
   :jnz (instr [tst offset] (jump-if offset #(not= 0 %) tst))})
