(ns advent-of-code.day01
  (:require [clojure.string :as str]
            [advent-of-code.utils :as u]))

(defn remove-letters
  [line]
  (filter #(Character/isDigit %) line))

(defn get-first-and-last
  [line]
  (str (first line) (last line)))

(defn day-1-part-1
  [filename]
  (->> (u/to-lines filename)
       (map remove-letters)
       (map get-first-and-last)
       (map #(Integer/parseInt %))
       (reduce +)))

(defn replace-words-with-nums
  "Parses the line and replaces instances of a written number with a string that
  has the integer in the middle and the written number on both ends. This prevents
  parsing errors when a string contains something like oneight. The string oneight
  should parse into 18, not just 1 or 8."
  [line]
  (let [digits {"one"   "one1one"
                "two"   "two2two"
                "three" "three3three"
                "four"  "four4four"
                "five"  "five5five"
                "six"   "six6six"
                "seven" "seven7seven"
                "eight" "eight8eight"
                "nine"  "nine9nine"}]
    (reduce
     (fn [acc v] (str/replace acc v (str (digits v))))
     line
     (keys digits))))

(defn day-1-part-2
  [filename]
  (->>
   (u/to-lines filename)
   (map replace-words-with-nums)
   (map remove-letters)
   (map get-first-and-last)
   (map parse-long)
   (reduce +)))

(day-1-part-1 "day01.txt")
(day-1-part-2 "day01.txt")
