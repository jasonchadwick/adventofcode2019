(ns advent2019.util)
; general functions that may be useful in more than one file


; zips coll1 and coll2 and maps f to new list
(defn zip-map-1 [f coll1 coll2 acc]
  (if (and (seq coll1) (seq coll2))
    (recur f (rest coll1) (rest coll2) (cons (f (first coll1) (first coll2)) acc))
    (reverse acc)))
(defn zip-map [f coll1 coll2]
  (zip-map-1 f coll1 coll2 nil))

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

(defn in-seq [x coll]
  (cond (empty? coll) false
        (= x (first coll)) true
        :else (recur x (rest coll))))

(defn zip-1 [coll1 coll2 acc]
  (if (or (empty? coll1) (empty? coll2))
    (reverse acc)
    (recur (rest coll1) (rest coll2) (cons `(~(first coll1) ~(first coll2)) acc))))
(defn zip [coll1 coll2]
  (zip-1 coll1 coll2 nil))

(defn seq-of [x length]
  (if (== length 0) nil
      (cons x (seq-of x (dec length)))))