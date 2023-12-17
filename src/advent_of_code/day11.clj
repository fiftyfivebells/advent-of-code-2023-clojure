(ns advent-of-code.day11
  (:require
   [clojure.math.combinatorics :as com]
   [advent-of-code.utils :as u]))

(def input "day11.txt")

(defn manhattan-distance
  "Distance between two points in a cartesian plane."
  [[px py] [qx qy]]
  (+ (abs (- px qx))
     (abs (- py qy))))

(defn find-universe-locs
  "Takes in the matrix and finds the x,y coordinates of each galaxy in the matrix."
  [universe]
  (let [max-x (count universe)
        max-y (count (first universe))]
    (for [x (range max-x), y (range max-y)
                 :when (= (get-in universe [x y]) \#)]
             [x y])))

(defn find-empty-rows
  "Takes a universe as input and finds the rows in the universe that have no galaxies.
  Returns the empty columns as a list of their indices in the universe."
  [universe]
  (let [max-x (count universe)]
    (for [x (range max-x)
                 :when (every? #(= % \.) (universe x))]
      x)))

(defn find-empty-cols
  "Takes a universe as input and finds the columns in the universe that have no galaxies.
  Returns the empty columns as a list of their indices in the universe."
  [universe]
  (let [max-y (count (first universe))]
    (for [y (range max-y)
          :when (every? #(= \. %) (map #(% y) universe))]
      y)))

(defn space-between
  "Takes two x or y coordinates and a list of the indices of empty rows or columns, then
  finds the space between the coordinates."
  [n m expanded]
  (let [x1 (min n m)
        x2 (max n m)]
    (count (filter #(< x1 % x2) expanded))))

(defn find-shortest-distance
  "Takes two points, the list of indices of empty rows and columns, and a multiplier as
  input. It then calculates the shortest distance between the two points using the given
  multiplier to determine the distance in the expanded universe."
  [p1 p2 empty-rows empty-cols multiplier]
  (let [rows-between (space-between (first p1) (first p2) empty-rows)
        cols-between (space-between (second p1) (second p2) empty-cols)]
    (+ (manhattan-distance p1 p2) (* multiplier rows-between) (* multiplier cols-between))))

(defn find-all-shortest-paths
  "Maps over every pair of coordinates in the universe and gives a list of the shortest
  paths between each one."
  [multiplier universe]
  (let [empty-rows (find-empty-rows universe)
        empty-cols (find-empty-cols universe)]
    (map #(find-shortest-distance (first %)
                                  (second %)
                                  empty-rows
                                  empty-cols
                                  multiplier)
         (com/combinations (find-universe-locs universe) 2))))

(defn day-11
  [input multiplier]
  (->> (u/read-file input)
       u/to-matrix
       (find-all-shortest-paths multiplier)
       (reduce +)))

(day-11 input 1)
(day-11 input 999999)
