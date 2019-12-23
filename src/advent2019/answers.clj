(ns ans
  (:require [advent2019.util])
  (:require [advent2019.intcode]))

(defn printans [day]
  (loop [upto day
         d 1]
    (println (str "Day " d ": " (load-file (str "src/advent2019/day" d ".clj"))))
    (if (= d upto) nil
        (recur upto (inc d)))))