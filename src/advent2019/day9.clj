(ns advent2019.day9
  (:require [advent2019.intcode :as intcode]))

`(~(intcode/run-to-end "resources/day9.txt" 0 0 '(1))
  ~(intcode/run-to-end "resources/day9.txt" 0 0 '(2)))