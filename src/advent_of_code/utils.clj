(ns advent-of-code.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-file
  [filename]
  (slurp (io/resource filename)))

(defn to-blocks
  [input]
  (str/split input #"\n\n"))

(defn to-lines
  [input]
  (str/split-lines input))

(defn to-matrix
  "Takes in string of lines and turns it into a matrix, with each character in its own index"
  [input]
  (->> input
       to-lines
       (mapv vec)))

(defn transpose-matrix
  "Basic matrix transpose operation: flips a matrix so its rows become its columns."
  [matrix]
  (apply mapv vector matrix))

(defn valid-coord?
  "Checks that the given coordinates into a matrix are "
  [row col max-row max-col]
  (and (< row max-row)
       (< col max-col)
       (>= row 0)
       (>= col 0)))

;; I didn't write this myself! After googling a bit trying to find out how to do what I
;; wanted, I found this on stack overflow: https://stackoverflow.com/a/3266877/6421
(defn re-pos
  "Return a list of pairs of (index, string) for all matches of `re` in `s`"
  [re s]
  (loop [m (re-matcher re s), res ()]
    (if (.find m)
      (recur m (cons (list (.start m) (.group m)) res))
      (reverse res))))

(defn strings->longs
  "Takes a list of strings and parses it into a list of longs using regex, so it will remove non-numeric characters."
  [los]
  (map parse-long (re-seq #"[-+]?\d+" los)))
