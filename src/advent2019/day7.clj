(ns advent2019.day7
  (:require [advent2019.intcode :as intcode])
  (:require [clojure.set :as set]))

(defn try-settings [fname settings input]
  (if (empty? settings) input
      (recur fname (rest settings)
             (intcode/run-to-end fname 0 `(~(first settings) ~input)))))

; works for all three examples, why not mine?
(defn try-all [fname]
  (let [nums #{0 1 2 3 4}]
    (apply 
     max
     (for [a nums
           b (set/difference nums #{a})
           c (set/difference nums #{a b})
           d (set/difference nums #{a b c})
           e (set/difference nums #{a b c d})]
       (try-settings fname `(~a ~b ~c ~d ~e) 0)))))


; part II

(defn try-settings-repeated [cur-amp amp-states]
  (let [output (apply intcode/run-code (nth amp-states cur-amp))
        next-amp (mod (inc cur-amp) 5)
        next-state (nth amp-states next-amp)]
   (if (and (== cur-amp 4) (number? output)) output
       (recur next-amp
              (assoc amp-states 
                     cur-amp output
                     next-amp `(~(first next-state) 
                                ~(second next-state) 
                                ~(concat (nth next-state 2) 
                                         `(~(if (number? output) output
                                                (first (first output)))))))))))

(defn try-all-repeated [fname]
  (let [nums #{5 6 7 8 9}]
    (apply max (for [a nums
                     b (set/difference nums #{a})
                     c (set/difference nums #{a b})
                     d (set/difference nums #{a b c})
                     e (set/difference nums #{a b c d})]
                 (try-settings-repeated 0 [`(~fname 0 (~a 0))
                                           `(~fname 0 (~b))
                                           `(~fname 0 (~c))
                                           `(~fname 0 (~d))
                                           `(~fname 0 (~e))])))))

`(~(try-all "resources/day7.txt") ~(try-all-repeated "resources/day7.txt"))