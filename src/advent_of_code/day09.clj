(ns advent-of-code.day09
  (:require
   [advent-of-code.utils :as u]))

(def input "day09.txt")

(defn sequence-differences
  "Gets the differences between each value in the history by partitioning by two with
  a step of 1 so that each partition starts with the next number in the sequence, then
  subtracts the previous number from the first and collects the differences."
  [history]
  (->>
   history
   (partition 2 1)
   (map #(apply - (reverse %)))))

(defn history-projection
  "Takes a history of a value and predicts the next value in history."
  [history]
  (loop [history history, finals 0]
    (if (every? zero? history)
      finals
      (recur (sequence-differences history) (+ finals (last history))))))

(defn day-09-part-1
  [input]
  (->>
   (u/read-file input)
   u/to-lines
   (map u/strings->longs)
   (map history-projection)
   (reduce +)))

;; Same as part 1, but I just reversed the lists before projecting
(defn day-09-part-2
  [input]
  (->>
   (u/read-file input)
   u/to-lines
   (map u/strings->longs)
   (map reverse)
   (map history-projection)
   (reduce +)))

(day-09-part-1 input)
(day-09-part-2 input)
