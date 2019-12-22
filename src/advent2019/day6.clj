(ns advent2019.day6
  (:require [advent2019.util :as util])
  (:require [clojure.java.io]))

; returns a map, keys are orbitING bodies, values are orbitED bodies 
; assumes all body names are 3 letters and format is ABC)DEF
(defn from-orbitlist [orbits acc-map]
  (if (empty? orbits) acc-map
      (let [center (subs (first orbits) 0 3)
            outer (subs (first orbits) 4)]
          (recur (rest orbits)
                 (assoc acc-map
                        outer (if (contains? acc-map outer)
                                (cons center (acc-map outer))
                                `(~center))
                        center (if (= center "COM")
                                 (cons nil `(~outer))
                                 (if (contains? acc-map center)
                                   (util/insert-second outer (acc-map center))
                                   `(~outer))))))))

; map format: key = body, value = (cons (body that it is orbiting)
;                                       (bodies that are orbiting it))

(defn map-from-file [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (let [orbits (line-seq rdr)]
      (from-orbitlist orbits nil))))

; total number of direct & indirect orbits for a single body
(defn orbits [o-map acc body]
  (if (= "COM" body)
    acc
    (recur o-map (inc acc) (first (o-map body)))))

(defn total-orbits [fname]
  (let [o-map (map-from-file fname)]
   (->> (keys o-map)
        (map (partial orbits o-map 0))
        (reduce + 0))))

; day 2

; BFS to find SAN from YOU
; returns path length or -1 if not found
; queue is a list of (body, distance)
(defn bfs-1 [o-map target visited queue]
  (if (empty? queue) -1
      (let [cur (first (first queue))
            dist (second (first queue))
            next (filter #(not (some identity 
                                     `(~(util/mem % visited)
                                       ~(util/mem % (map first queue)))))
                         (o-map cur))]
        (if (util/in-seq target next) (inc dist)
            (recur o-map target 
                   (cons cur visited) 
                   (concat (util/zip next 
                                     (util/repeated-seq (inc dist) (count next)))
                           (rest queue)))))))

(defn bfs-from-file [fname start target]
  (bfs-1 (map-from-file fname) target nil `((~start 0))))

; answers for both parts
`(~(total-orbits "resources/day6.txt") 
  ~(- (bfs-from-file "resources/day6.txt" "YOU" "SAN") 2))
