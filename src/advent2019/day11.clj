(ns advent2019.day11
  (:require [advent2019.intcode :as intcode]))

; prev-painted: list of ((x,y), color) color: 0 black, 1 white
(defn paint-wall-1 [robot-pos robot-dir prev-painted v pos rel inputs init]
  (let [color-output (intcode/run-code v pos rel inputs init)
        turn-output (if (number? color-output) nil
                        (apply intcode/run-code color-output))]
    (println robot-pos)
    (if (number? color-output) prev-painted
        (let [new-dir (mod (+ robot-dir
                              (dec (* 2 (first (first turn-output))))) 
                           4)
              movement (case new-dir
                         0 '(1 0)
                         1 '(0 1)
                         2 '(-1 0)
                         3 '(0 -1))
              new-pos `(~(+ (first robot-pos) (first movement))
                        ~(+ (second robot-pos) (second movement)))]
          (recur new-pos new-dir (assoc prev-painted robot-pos (first (first color-output)))
                 (first turn-output) (second turn-output) (nth turn-output 2)
                 `(~(if-let [color (prev-painted new-pos)] color 0))
                 false)))))

(defn paint-wall [fname inputs]
  (paint-wall-1 '(0 0) 0 {} fname 0 0 inputs true))