(ns advent-of-code.day13
  (:require
   [clojure.string :as str]
   [advent-of-code.utils :as u]))

(def input "day13.txt")

(defn row-diff
  "Compares each character of the two rows, giving a 0 if they're equal and a 1 if
  they're not equal."
  [[x y]]
  (reduce (fn [acc [a b]] (+ acc (if (= a b) 0 1))) 0 (map vector x y)))

(defn is-reflection?
  "Takes in two matrices and a number of smudges and compares them to each other row
  by row. Checks the difference between each row (accounting for number of smudges) and
  returns true if the matrices are reflections of each other."
  [[top bottom] smudges]
  (let [rtop (reverse top)]
    (if (or (empty? top)
            (empty? bottom))
      false
      (= smudges (reduce + (map row-diff (map vector rtop bottom)))))))

(defn find-mirror-rows
  "Iterates over the given matrix and tests for reflection at each row index. Returns the
  index of the reflection if one is found, or -1 otherwise."
  [smudges matrix]
  (let [max-x (count matrix)]
    (loop [[x & xs] (range max-x)]
      (cond
        (nil? x) -1
        (is-reflection? (split-at x matrix) smudges) x
        :else (recur xs)))))

(defn find-reflection
  "Takes a matrix and number of smudges, then finds the reflection point. It first checks
  the rows of the matrix and if it doesn't find a reflection, it transposes the matrix
  and checks the rows of the transposition."
  [smudges matrix]
  (let [reflection (find-mirror-rows smudges matrix)]
    (if (pos? reflection)
      (* 100 reflection)
      (find-mirror-rows smudges (u/transpose-matrix matrix)))))

(defn day-13-part-1
  [input]
  (->> input
       u/read-file
       u/to-blocks
       (map #(mapv vec (str/split-lines %)))
       (map #(find-reflection 0 %))
       (reduce +)))

(defn day-13-part-2
  [input]
  (->> input
       u/read-file
       u/to-blocks
       (map u/to-matrix)
       (map #(find-reflection 1 %))
       (reduce +)))

(day-13-part-1 input)
(day-13-part-2 input)
