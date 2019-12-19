;intcode functions, used by several days

(ns advent2019.intcode
  (:require [advent2019.util :as util])
  (:require [clojure.java.io])
  (:require [clojure.string]))

; operates on the two next elements of an int vector and stores result
; in index specified by third element
(defn operate [op v pos]
  (assoc v (nth v (+ 3 pos)) (op (nth v (nth v (+ 1 pos)))
                                 (nth v (nth v (+ 2 pos))))))

(defn make-vec [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (vec (map (fn [s] (Integer/parseInt s)) (clojure.string/split (slurp rdr) #",")))))

(defn run-code-1 [v pos]
  (if (>= pos (count v))
    (throw (Exception. "oh no"))
    (let [opcode (mod (nth v pos) 100)]
     (case opcode
      99 (nth v 0)
      1 (recur (operate + v pos) (+ 4 pos))
      2 (recur (operate * v pos) (+ 4 pos))
      3 (recur (assoc v (nth v (+ 1 pos)) (nth v 0)) (+ 2 pos))
      4 (recur (assoc v 0 (nth v (nth v (+ 1 pos)))) (+ 2 pos))))))

(defn run-code [v]
  (run-code-1 v 0))

(defn run-from-file [fname]
  (run-code-1 (make-vec fname) 0))