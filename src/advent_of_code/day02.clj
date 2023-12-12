(ns advent-of-code.day02
  (:require
   [clojure.string :as str]
   [advent-of-code.utils :as u]))

(defn remove-game-from-line
  [line]
  (rest (str/split line #":")))

(defn game->rounds
  [game]
  (map str/trim (str/split (first game) #";")))

(defn round->pairs
  "Takes a string of rounds and turns it into a list of pairs, where each pair
  represents a round."
  [round]
  (flatten (map #(str/split % #",") round)))

(defn string->map
  "Turns a list of strings representing rounds into a list of pairs (ex. '(:green 7))."
  [s]
  (let [trimmed (str/trim s)
        pair    (str/split trimmed #" ")]
    (list (keyword (second pair)) (Integer/parseInt (first pair)))))

(defn line-to-lists
  [line]
  (map string->map line))

(defn check-value
  [value]
  (let [game-values {:red   12
                     :green 13
                     :blue  14}
        [color count] value]
    (<= count (game-values color))))

(defn transform-input
  "Reads the file line by line and transforms the input into a list of lists of lists,
  ie.
  '(
    ((blue 14) (red 3)) ((green 7) (red 3) (blue 3))
    ((red 1) (green 3) (red 2))
  )"
  [input]
  (->>
   (u/to-lines input)
   (map remove-game-from-line)
   (map game->rounds)
   (map round->pairs)
   (map line-to-lists)))

(defn legal-game?
  [game]
  (every? check-value game))

(defn zip-results-with-game-number
  [games-list]
  (map list (iterate inc 1) games-list))

(defn remove-impossible
  [games]
  (filter #(second %) games))

(defn part-1-result
  [filename]
  (->>
   (transform-input filename)
   (map legal-game?)
   (zip-results-with-game-number)
   (remove-impossible)
   (reduce #(+ %1 (first %2)) 0)))

(defn compare-values
  "Takes a map and key/value pair as inputs. If the value of the pair is greater than
  the value associated with the key in the map, it replaces the map's value with the new
  one. Otherwise, it returns the map unchanged."
  [acc pair]
  (let [[color n] pair]
    (if (> n (acc color))
      (assoc acc color n)
      acc)))

(defn find-mins
  [round]
  (reduce compare-values {:red 0, :blue 0, :green 0} round))

(defn cube-round
  [round]
  (reduce * (map #(val %) (find-mins round))))

(defn part-2-result
  [filename]
  (->>
  (transform-input filename)
  (map find-mins)
  (map cube-round)
  (reduce +)))

(part-1-result "day02.txt")
(part-2-result "day02.txt")
