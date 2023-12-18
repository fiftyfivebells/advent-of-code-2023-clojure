(ns advent-of-code.day14
  (:require
   [clojure.string :as str]
   [advent-of-code.utils :as u]))

(def input "day14.txt")

(defn rotate-platform
  [matrix]
  (map #(apply str %) (u/transpose-matrix matrix)))

;; My original implementation for this was basically a modified insertion sort. This
;; is certainly way cleaner and it seems like it runs a tad faster as well
(defn roll-row-west
  "Takes the row, splits it on the # character, then sorts, reverses, and stringifies the
  row, then joins it back together on the #."
  [row]
  (->> (str/split row #"#" -1)
       (map (comp #(apply str %) reverse sort))
       (str/join "#")))

(defn roll-row-east
  "Similar to the west roll, takes the row, splits on the #, sorts it (but doesn't
  reverse it) and stringifies it, then joins it back together with # chars."
  [row]
  (->> (str/split row #"#" -1)
       (map (comp #(apply str %) sort))
       (str/join "#")))

(defn roll-east
  "Rolls an entire platform east. This is just mapping the roll-row-east function over
  the whole platform."
  [platform]
  (map roll-row-east platform))

(defn roll-west
  "Rolls an entire platform west. This is just mapping the roll-row-west function over
  the whole platform."
  [platform]
  (map roll-row-west platform))

(defn roll-north
  "Takes in a platform, transposes it, then rolls everything west and transposes it back."
  [platform]
  (->> platform
       rotate-platform
       roll-west
       rotate-platform))

(defn roll-south
  "Takes in a platform, transposes it, then rolls everything east and transposes it back."
  [platform]
  (->> platform
       rotate-platform
       roll-east
       rotate-platform))

(defn calculate-row-load
  "Calculates the load of one row by taking in the row and its index in the platform,
  then counting the number of 'rocks' (O chars) and multiplying by the row."
  [i row]
  (let [rocks (count (filter #(= \O %) row))]
    (* rocks i)))

(defn calculate-load
  "Maps over the platform and calculates the load for each row, then sums the rows loads."
  [platform]
  (reduce + (for [i (range 0 (count platform))]
              (calculate-row-load (- (count platform) i) (nth platform i)))))

(defn find-after-cycle
  "Used after the spin-cycle function finds a cycle in the iteration. It takes a map from
  a platform to its corresponding index, the index of the current platform, the current
  index into the spin cycle, and the number being targeted. It then uses modulo division
  to find the key in the table that corresponds to the platform at n spin cycles. It
  basically works by taking the target n - the current index in the spin, and then doing
  the mod of that by the index where the cycle occurs."
  [seen->idx platform-idx curr-idx target-num]
  (let [cycle-location (- curr-idx platform-idx)  ;; the index where the cycle occurs
        to-end (mod (- target-num curr-idx) cycle-location)]
    (seen->idx (+ to-end platform-idx))))

(defn spin
  "One full rotation: rolls nort, west, south, and then east."
  [platform]
  (-> platform
       roll-north
       roll-west
       roll-south
       roll-east))

(defn spin-cycle
  "Takes a number of cycles and the starting platform. It then spins repeatedly until it
  either reaches n or finds a cycle (the same platform formation twice) and then uses
  find-after-cycle to get the platform formation at n."
  [n platform]
  (loop [[x & xs] (iterate spin platform), i 0, seen {}, idx->seen {}]
    (cond
      (= i n) x
      (seen x) (find-after-cycle idx->seen (seen x) i n)
      :else (recur xs (inc i) (assoc seen x i) (assoc idx->seen i x)))))

(defn day-14-part-1
  [input]
  (->> input
       (u/read-file)
       u/to-lines
       roll-north
       calculate-load))

(defn day-14-part-2
  [input]
  (->> input
       (u/read-file)
       u/to-lines
       (spin-cycle 1000000000)
       calculate-load))

(day-14-part-1 input)
(day-14-part-2 input)
