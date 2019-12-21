(ns advent2019.day5
  (:require [advent2019.intcode :as intcode]))

; (most of day 5 code was written in intcode.clj)

`(~(first (intcode/run-from-file "resources/day5.txt" 0 '(1)))
  ~(first (intcode/run-from-file "resources/day5.txt" 0 '(5))))