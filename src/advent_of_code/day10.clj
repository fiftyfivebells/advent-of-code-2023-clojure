(ns advent-of-code.day10
  (:require
   [advent-of-code.utils :as u]
   [clojure.string :as str]))

(def input "day10.txt")

(defn valid-coords [[y x] max-y max-x]
  (and (< y max-y)
       (< x max-x)
       (>= y 0)
       (>= x 0)))

(defn directions
  "Defines the two locations that can be reached by each symbol."
  [c y x max-y max-x]
  (case c
    \| (filter #(valid-coords % max-y max-x) (list [(dec y) x] [(inc y) x]))
    \- (filter #(valid-coords % max-y max-x) (list [y (dec x)] [y (inc x)]))
    \L (filter #(valid-coords % max-y max-x) (list [(dec y) x] [y (inc x)]))
    \J (filter #(valid-coords % max-y max-x) (list [y (dec x)] [(dec y) x]))
    \7 (filter #(valid-coords % max-y max-x) (list [y (dec x)] [(inc y) x]))
    \F (filter #(valid-coords % max-y max-x) (list [y (inc x)] [(inc y) x]))
    \. ()
    \S (list \S)))

(defn build-graph
  "Turns the matrix into a graph starting at the S node and following all of the paths."
  [matrix]
  (let [max-y (count matrix)
        max-x (count (first matrix))]
    (into {} (for [y (range max-y)
                   x (range max-x)]
               {[y x] (directions (get-in matrix [y x]) y x max-y max-x)}))))

(defn contains-coords? [loc val]
  (if (some #(= val %) loc) true false))

(defn bfs
  "Takes a graph and does a breadth-first search of all nodes starting at S. It adds the
  neighbors of the current node into a queue and decides which node to visit by pulling
  from the queue. Returns a map with coordinates as keys and the number of steps from
  the S node as the values."
  [graph]
  (let [start           (first (filter #(= (graph %) '(\S)) (keys graph)))
        start-neighbors (filter #(contains-coords? (graph %) start) (keys graph))]
    (loop [queue (map #(vec [% 1]) start-neighbors), visited {start 0}]
      (if (empty? queue)
        visited
        (let [[node distance] (first queue)
              next-node (filter #(not (contains? visited %)) (graph node))]
          (recur (concat (rest queue) (map #(vec [% (inc distance)]) next-node))
                 (assoc visited node distance)))))))

(defn day-10-part-1
  [input]
  (->>
   input
   u/to-matrix
   build-graph
   bfs
   vals
   (reduce max)))

(defn remove-unvisited
  "Creates a new matrix by removing any unvisited nodes from the matrix."
  [visited matrix]
  (let [max-y (count matrix)
        max-x (count (first matrix))]
    (partition max-x (for [y (range max-y), x (range max-x)]
                       (if (visited [y x])
                         (get-in matrix [y x])
                         \.)))))

(defn count-enclosed
  "Counts up the squares enclosed by the loop. It does this by replacing all of the
  symbols outside the loop with empty strings, then turning remaining symbols into the
  | character. After that, the . characters in between the | characters can just be
  counted up."
  [row]
  (let [row (str/replace row "S" "J")  ;; necessary to know where to go from the start
        row (str/replace row #"F-*7|L-*J" "")
        row (str/replace row #"F-*J|L-*7" "|")]
    (loop [[c & cs] row, enclosed 0, total 0]
      (cond
        (nil? c) total
        (= c \|) (recur cs (inc enclosed) total)
        :else    (recur cs enclosed (if (and (= c \.)
                                             (odd? enclosed))
                                      (inc total)
                                      total))))))

(defn day-10-part-2
  [input]
  (let [matrix  (u/to-matrix input)
        graph   (build-graph matrix)
        visited (bfs graph)]
    (reduce + (map #(count-enclosed (apply str %)) (remove-unvisited visited matrix)))))

(day-10-part-1 input)
(day-10-part-2 input)
