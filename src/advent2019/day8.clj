(ns advent2019.day8
  (:require [clojure.java.io])
  (:require [advent2019.util :as util])
  (:require [clojure.string]))

(defn fewest-zeroes [string w h]
  (let [size (* w h)
        num (/ (count string) size)]
    (reduce #(if (<= (first %1) (first %2)) %1 %2) 
            (for [substr (map #(subs string % (+ % size))
                              (util/tabulate #(* % size) num))]
              `(~(util/count-char substr \0) ~substr)))))

(defn mult-digits [fname w h]
  (let [fewest (fewest-zeroes (slurp fname) w h)]
    (* (util/count-char (second fewest) \1)
       (util/count-char (second fewest) \2))))


; part 2

(defn pixels-at-pos [image w h i]
  (let [size (* w h)
        num (/ (count image) size)]
    (loop [n (dec num)
           acc nil]
      (if (< n 0) acc (recur (dec n) (cons (nth image (+ (* n size) i)) acc))))))

(defn visible-pixel [str]
  (cond (empty? str) -1
        (= \2 (first str)) (recur (rest str))
        :else (first str)))

(defn visible-image [fname w h]
  (let [img (slurp fname)]
    (apply str (map #(visible-pixel (pixels-at-pos img w h %)) 
                    (util/tabulate identity (* w h))))))

(defn write-output [string w]
  (with-open [wr (clojure.java.io/writer "resources/day8-message.txt" :append true)]
    (loop [s (clojure.string/replace string \1 \#)]
      (if (empty? s) nil
          (do
            (.write wr (str (subs s 0 (min w (count s))) \return))
            (recur (subs s (min w (count s)) (count s))))))))

(mult-digits "resources/day8.txt" 25 6)