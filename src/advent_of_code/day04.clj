(ns advent-of-code.day04
  (:require
   [clojure.string :as str]
   [clojure.math :as math]
   [advent-of-code.utils :as u]))

(defn card->num-pairs
  [card]
  (let [[winners numbers] (str/split card #"\s+[|]\s+")
        winner-longs (u/strings->longs winners)
        number-longs (u/strings->longs numbers)]
    ;; I take rest of winner-longs because the first element of that list is going
    ;; to be the card number, ie. if its "Card 1: ...", the first element is 1 and
    ;; I don't want that
    (list (rest winner-longs) number-longs)))

(defn get-matching-numbers-count
  [card-numbers]
  (let [[winners numbers] card-numbers
        winner-set (set winners)]
    (count (filter winner-set numbers))))

(defn day-4-part-1
  [input]
  (->>
   (u/to-lines input)
   (map card->num-pairs)
   (map get-matching-numbers-count)
   (filter pos?)
   (map dec)  ;; this fixes the problem of not doubling the first win
   (map #(math/pow 2 %))  
   (reduce +)))

(defn update-card-totals
  "Updates the count of the given card with the total number of new wins."
  [card-totals i matched]
  ;; iterate over the range of matching numbers and update the counts starting
  ;; with the current card
  (loop [[x & xs] (range matched) card-totals card-totals]
    (if (nil? x)
      card-totals
      (recur xs (update card-totals (+ i x 1) + (card-totals i))))))

(defn count-of-cards
  "Iterates through a list of card games and counts the number of each card."
  [cards]
  ;; makes a vector of card totals indexed by the given cards (with 1 for each count to start)
  (let [card-totals (vec (repeat (count cards) 1))]
    ;; loop over the cards one by one, updating the card totals depending on how many winning
    ;; numbers the current card has
    (loop [[i & is] (range (count cards))
           card-totals card-totals]
      (if (nil? i)
        (reduce + card-totals)
        (recur is (update-card-totals card-totals i (get-matching-numbers-count (cards i))))))))

(defn day-4-part-2
  [input]
  (->>
   (u/to-lines input)
   (mapv card->num-pairs)
   count-of-cards))

(day-4-part-1 "day04.txt")
(day-4-part-2 "day04.txt")
