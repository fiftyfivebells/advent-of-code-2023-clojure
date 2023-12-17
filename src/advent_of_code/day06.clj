(ns advent-of-code.day06
  (:require [advent-of-code.utils :as u]))

(def input "day06.txt")

(defn zip-input-list
  "Zips the two inputs together to make race time/race record pairs."
  [input]
  (map vector (first input) (second input)))

(defn record-breaker?
  [time max-time record]
  (> (* time (- max-time time)) record))

(defn get-record-breakers
  "Loops over the list of times and distances and finds all the times that will break
  the race's record."
  [pair]
  (let [[time record] pair]
    (loop [[x & xs] (range time), scores ()]
      (cond
        (nil? x) scores
        (record-breaker? x time record) (recur xs (cons 1 scores))
        :else (recur xs scores)))))

(defn day-6-part-1
  [input]
  (->>
   (u/read-file input)
   u/to-lines
   (map #(u/strings->longs %))
   (zip-input-list)
   (map get-record-breakers)
   (map #(count %))
   (reduce *)))

;; Part 2 requires the input to be manipulated a little bit

(defn squish-input
  "For part two, the input needs to have each line's numbers squished into one large
  number per line. This function parses the digits out of each line, concatenates them
  into a list of strings, parses list of strings into a list of longs, and then flattens
  the list into the two final numbers."
  [input]
  (->>
   (u/read-file input)
   u/to-lines
   (map #(re-seq #"\d+" %))
   (map #(reduce str %))
   (map #(u/strings->longs %))
   flatten))

(defn day-6-part-2
  [input]
  (->>
   input
   squish-input
   get-record-breakers
   count))

(day-6-part-1 input)
(day-6-part-2 input)
