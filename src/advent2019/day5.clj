(ns advent2019.day5
  (:require [advent2019.intcode]))

; (most of day 5 code was written in intcode.clj)

`(~(advent2019.intcode/run-from-file "resources/day5.txt" 1)
  ~(advent2019.intcode/run-from-file "resources/day5.txt" 5))