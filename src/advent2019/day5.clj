(ns advent2019.day5
  (:require [advent2019.intcode :as intcode]))

; (most of day 5 code was written in intcode.clj)

`(~(intcode/run-to-end "resources/day5.txt" 0 0 '(1))
  ~(intcode/run-to-end "resources/day5.txt" 0 0 '(5)))