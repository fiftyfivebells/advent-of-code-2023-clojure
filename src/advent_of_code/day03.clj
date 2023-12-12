(ns advent-of-code.day03
  (:require
   [advent-of-code.utils :as u]))

(def input "day03.txt")

(defn get-number-locations
  "Takes in a string of lines and finds the index of the start of each number"
  [input]
  (mapv #(u/re-pos #"\d+" %) (u/to-lines input)))

(defn is-part?
  [c]
  (not (#{\1 \2 \3 \4 \5 \6 \7 \8 \9 \0 \.} c)))

(defn is-part-adjacent?
  "Takes a matrix, coordinates, and a number as input and checks to see if there
  are any parts adjacent to the number. Returns true if yes, false otherwise."
  [matrix row col max-y max-x num]
  (some true? (for [i (range (dec row) (+ row 2))
                    j (range (dec col) (inc (+ col (count num))))]
                (and (u/valid-coord? i j max-y max-x)
                     (is-part? (get-in matrix [i j]))))))

(defn get-part-adjacent-numbers
  "Loops through all of the matrix locations where there is a number and checks
  whether they are adjacent to a part. If yes, it collects that number into a list."
  [matrix locations]
  (let [max-y (count matrix)
        max-x (count (first matrix))]
    (map parse-long
         (for [row (range (count locations)), [col num] (locations row)
               :when (is-part-adjacent? matrix row col max-y max-x num)]
           num))))

(defn day-3-part-1
  "Using the puzzle input, it creates the matrix and number locations, then finds
  all of the numbers with adjacent parts, and adds the list of numbers."
  [input]
  (let [matrix    (u/to-matrix input)
        locations (get-number-locations input)]
    (reduce + (get-part-adjacent-numbers matrix locations))))

;;;;; part 2

(defn get-gears-around-num
  "Takes a matrix, coordinates, and a number as input and then determines whether
  there are any gears (*) around the number. If yes, it gathers the number and the
  coordinates of the gear into a list. Returns this list (empty or not)."
  [matrix row col max-y max-x num]
  (for [i (range (dec row) (+ row 2))
        j (range (dec col) (inc (+ col (count num))))
        :when (and (u/valid-coord? i j max-y max-x)
                   (= (get-in matrix [i j]) \*))]
    (list (parse-long num) [i j])))

(defn find-all-gears
  "Takes the matrix and list of numbers as inputs, then loops over the list of numbers
  to find all gears that are adjacent to numbers. Returns a map with coordinates
  as keys and adjacent numbers as values."
  [matrix locations]
  (let [max-y (count matrix)
        max-x (count (first matrix))]
    (group-by last
              (map first
                   (filter seq
                           (for [row (range (count locations)), [col num] (locations row)]
                             (get-gears-around-num matrix row col max-y max-x num)))))))

(defn calculate-ratios
  "Takes the list of all gears, filters out the gears that do not have two numbers
  adjacent, and then calculates the gear ratio by multiplying the two numbers adjacent
  to each gear."
  [gears]
  (let [gear-map (filter #(= 2 (count %)) (vals gears))]
    (map #(* (first (first %)) (first (second %))) gear-map)))

(defn day-3-part-2
  "Turns the puzzle input into a matrix and list of locations, finds all the gears,
  calculates all the gear ratios, and then adds the list of ratios together."
  [input]
  (let [matrix    (u/to-matrix input)
        locations (get-number-locations input)]
    (->>
     (find-all-gears matrix locations)
     calculate-ratios
     (reduce +))))

(day-3-part-1 input)
(day-3-part-2 input)
