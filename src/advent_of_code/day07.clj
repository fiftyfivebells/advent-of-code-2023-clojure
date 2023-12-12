(ns advent-of-code.day07
  (:require
   [clojure.string :as str]
   [advent-of-code.utils :as u]))

(def input "day07.txt")

(defn string->hand
  "Associates a string representing a hand (ie. 'QK88A') into a hash map of the hand's
  keys and their counts (ie. {Q: 1 K: 1 8: 2 A: 1})."
  [hand-string]
  (->>
   hand-string
   (group-by identity)
   (map #(hash-map (first %) (count (second %))))
   (into {})))

;; These two functions help determine what kind of hand we have
(defn three-of-a-kind?
  [hand]
  (= 3 (last (sort (vals hand)))))

(defn full-house?
  [hand]
  (= (list 2 3) (sort (vals hand))))

(defn js->jokers
  "This function takes a hand and accounts for the J cards by turning them into
  wildcards. Removes the X from the hand and then adds its count to the card in the
  hand that we have most of (ie. {T: 3 X: 1 A: 1} -> {T: 4 A: 1})."
  [hand]
  (let [jokers   (hand \X)
        new-hand (dissoc hand \X)
        top-key  (last (sort-by new-hand (keys new-hand)))]
    (if top-key
      (update new-hand top-key + jokers)
      {\X 5})))

(defn classify-hand
  "Takes the hand and gives it a type depending on the breakdown of cards in the hand."
  [hand]
  (let [hand (string->hand hand)
        new-hand (if (pos? (get hand \X 0))
                   (js->jokers hand)
                   hand)
        keys (count (keys new-hand))]
    (cond
      (= keys 1) 7
      (= keys 2) (if (full-house? new-hand) 5 6)
      (= keys 3) (if (three-of-a-kind? new-hand) 4 3)
      (= keys 4) 2
      (= keys 5) 1)))

(defn parse-hands
  "Takes the puzzle input as a parameter and parses it into a list of hash-maps that
  each represent a hand (ie. {:hand QKAA6 :bid 756 :type 2})."
  [input]
  (let [hands (map #(str/split % #"\s+") input)]
    (map #(hash-map :hand (first %)
                    :bid (parse-long (second %))
                    :type (classify-hand (first %))) hands)))

(defn compare-hands
  "Compares 2 hands starting at the first value and looping over them together."
  [h1 h2]
  (let [card-values {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14 \X 1}]
    (loop [[x & xs] (:hand h1), [y & ys] (:hand h2)]
      (cond
        (nil? x) 0
        (= x y) (recur xs ys)
        :else (- (card-values x) (card-values y))))))

(defn sort-hands
  [loh]
  (sort compare-hands loh))

(defn score-hand
  [hand rank]
  (* (hand :bid) rank))

(defn rank-hands-by-type
  "Takes a list of hands and ranks them in ascending order. It does this by grouping the
  hands by their type, sorting the types in ascending order, then sorting the hands that
  correspond to each time, and finally concatenating the lists into a sorted list of hands."
  [hands]
  (let [grouped-hands (group-by :type hands)]
    (->>
     grouped-hands
     keys
     sort
     (map #(grouped-hands %))
     (map sort-hands)
     (reduce concat))))

(defn score-all-hands
  "Takes a list of hands, ranks them by type, then maps over that list and an infinite
  sequence starting at 1. This supplies the multiplier by which to multiply the hand's
  bid by."
  [hands]
  (map score-hand
       (rank-hands-by-type hands)
       (iterate inc 1)))

(defn day-07-part-1
  [input]
  (->>
   (u/to-lines input)
   parse-hands
   rank-hands-by-type
   score-all-hands
   (reduce +)))

(defn day-07-part-2
  [input]
  (->>
   (u/to-lines input)
   (map #(str/replace % \J \X))
   parse-hands
   rank-hands-by-type
   score-all-hands
   (reduce +)))

(day-07-part-1 input)
(day-07-part-2 input)
