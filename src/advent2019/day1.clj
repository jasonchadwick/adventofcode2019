(ns advent2019.day1
  (:require [clojure.java.io]))

(defn fuel [mass]
  (if (< mass 9) 0
      (let [new-fuel (- (quot mass 3) 2)]
        (+ new-fuel (fuel new-fuel)))))

;part II
(defn add-fuels [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (reduce + (map #(fuel (Integer/parseInt %))
               (line-seq rdr)))))

;answer for part 2
`(~(add-fuels "resources/day1.txt"))