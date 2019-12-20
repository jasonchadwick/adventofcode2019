(ns advent2019.answers
  (:require [advent2019.util])
  (:require [advent2019.intcode]))

(def daynum 6)

(defn print-answers-1 [upto day]
  (println (str "Day " day ": " (load-file (str "src/advent2019/day" day ".clj"))))
  (if (= day upto) nil
      (recur upto (inc day))))
(defn print-answers [day]
  (print-answers-1 day 1))