(defn hello [coll]
  (map (fn add1 [x] (+ 1 x)) coll))