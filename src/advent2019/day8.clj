(ns advent2019.day8
  (:require [clojure.java.io])
  (:require [advent2019.util :as util]))

(defn fewest-zeroes [fname w h]
  (let [string (slurp fname)
        size (* w h)
        num (/ (count string) size)]
    (reduce #(if (<= (first %1) (first %2)) %1 %2) 
            (for [substr (map #(subs string % (+ % size))
                              (util/tabulate #(* % size) num))]
              `(~(util/count-char substr \0) ~substr)))))

(defn mult-digits [fname w h]
  (let [fewest (fewest-zeroes fname w h)]
    (* (util/count-char (second fewest) \1)
       (util/count-char (second fewest) \2))))

(mult-digits "resources/day8.txt" 25 6)