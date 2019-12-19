; general functions that may be useful in more than one file

(ns advent2019.util)

; zips coll1 and coll2 and maps f to new list
(defn zip-map [f coll1 coll2 acc]
  (if (seq coll1)
    (recur f (rest coll1) (rest coll2) (cons (f (first coll1) (first coll2)) acc))
    acc))

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
  (mod (quot x (Math/pow 10 n)) 10))