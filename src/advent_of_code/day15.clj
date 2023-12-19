(ns advent-of-code.day15
  (:require
   [clojure.string :as str]
   [flatland.ordered.map :refer [ordered-map]]
   [advent-of-code.utils :as u]))

(def input "day15.txt")

(defn prepare-input
  [input]
  (str/split (str/trim-newline (u/read-file input)) #","))

(defn hash-algorithm
  [acc value]
  (let [add  (+ (int value) acc)
        mult (* add 17)]
    (rem mult 256)))

(defn HASH
  "Takes in the input and runs the provided hashing algorithm on each step of the
  instruction list."
  [input]
  (reduce hash-algorithm 0 input))

(defn day-15-part-1
  [input]
  (->> input
       prepare-input
       (map HASH)
       (reduce +)))

(defn follow-instructions
  "Takes the list of instructions as input. For each instruction, it determines whether
  it's an add or remove of a lens into the box. It gives the box number determined by the
  hash function, the label of the lens, its operation, and the focal power (if
  applicable) and stores them in a map."
  [instruction]
  (if (str/ends-with? instruction "-")
    (let [instruction (drop-last instruction)]
      {:label (apply str instruction)
       :box-no (HASH instruction)
       :op dissoc})
    (let [instr (drop-last 2 instruction)]
      {:label (apply str instr)
       :box-no (HASH instr)
       :op assoc
       :power (parse-long (str (last instruction)))})))

(defn execute-instruction
  [boxes {:keys [label box-no op power]}]
  (update boxes box-no op label power))

(defn HASHMAP
  "Runs the execute-instructions function on each instruction created by the
  follow-instructions function. It performs the operation specified by the instruction
  and stores the results in the corresponding box in a 256-length vector."
  [instructions]
  (reduce execute-instruction (vec (repeat 256 (ordered-map))) instructions))

(defn calc-focal-power
  "Takes in the list of boxes and calculates the focal power using the algorithm given
  by the puzzle description."
  [boxes]
  (for [[i box] (map-indexed vector boxes)
        [j [_ power]] (map-indexed vector box)]
    (* (inc i) (inc j) power)))

(defn day-15-part-2
  [input]
  (->> input
       prepare-input
       (map follow-instructions)
       HASHMAP
       calc-focal-power
       (reduce +)))

(day-15-part-1 input)
(day-15-part-2 input)
