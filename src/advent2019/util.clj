(ns advent2019.util
  (:require [clojure.string :as string]))
; general functions that may be useful in more than one file


; zips coll1 and coll2 and maps f to new list
(defn zip-map [f coll1 coll2]
  (loop [c1 coll1
         c2 coll2
         acc nil]
    (if (and (seq c1) (seq c2))
      (recur (rest c1) (rest c2) (cons (f (first c1) (first c2)) acc))
      (reverse acc))))

; true if elem is in coll, false otherwise
(defn mem [elem coll]
  (cond (empty? coll) false
        (= elem (first coll)) true
        :else (recur elem (rest coll))))

; finds first elem in coll that satisfies p. Helper for lazy-filter.
(defn first-sat [p coll]
  (cond (empty? coll) nil
        (p (first coll)) `(~(first coll) ~(rest coll))
        :else (recur p (rest coll))))

; lazy version of filtering a coll
(defn lazy-filter [p coll]
  (lazy-seq (let [next (first-sat p coll)]
              (if (nil? next) '()
                  (cons (first next)
                        (lazy-filter p (second next)))))))

; nth (from right) digit of x
(defn digit [x n]
  (int (mod (quot x (Math/pow 10 n)) 10)))

(defn mapreduce
  ([f g coll]
   (reduce g (map f coll)))
  ([f g z coll]
   (reduce g z (map f coll))))

(defn insert-second [x coll]
  (cons (first coll) (cons x (rest coll))))

(defn zip [coll1 coll2]
  (loop [c1 coll1
         c2 coll2
         acc nil]
    (if (or (empty? c1) (empty? c2))
      (reverse acc)
      (recur (rest c1) (rest c2) (cons `(~(first c1) ~(first c2)) acc)))))

(defn tabulate [f length]
  (loop [n (dec length) acc nil]
    (if (< n 0) acc
        (recur (dec n) (cons (f n) acc)))))

(defn repeated-seq [x length]
  (loop [n length
         acc nil]
    (if (== 0 n) acc
        (recur (dec n) (cons x acc)))))

(defn safe-cdr [coll backup]
  (if (empty? coll) backup (rest coll)))

(defn subcoll [x start end]
  (loop [y x
         s start
         e end
         acc nil]
    (if (== s e) acc
        (recur x s (dec e) (cons (nth y e) acc)))))

(defn count-char [string c]
  (loop [str string acc 0]
    (let [idx (string/index-of str c)]
      (if (nil? idx) acc
          (recur (subs str (inc idx)) (inc acc))))))

(defn dprint [s x]
  (if (= s 't) (println x) nil)
  x)

(defn only-or [x y]
  (and (not (and x y))
       (or x y)))

(defn diff-signs [x y]
  (only-or (not (= (Math/abs x) x))
      (not (= (Math/abs y) y))))

(defn prod [coll1 coll2]
  (for [c1 coll1
        c2 coll2]
    `(~c1 ~c2)))

; makes a list where each key points to a list of vals
; i.e. duplicate keys add an entry to the list of that key
; lists are sorted based on cmp.
(defn mk-assoc-list [keys vals sortfn]
  (loop [ks keys
         vs vals
         acc {}]
    (if (empty? ks) acc
        (recur (rest ks) (rest vs)
               (if (contains? acc (first ks))
                 (assoc acc (first ks) (sortfn (cons (first vs) (get acc (first ks)))))
                 (assoc acc (first ks) `(~(first vs))))))))

(defn dist [x y]
  (Math/sqrt (+ (* x x) (* y y))))