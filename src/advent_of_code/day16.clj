(ns advent-of-code.day16
  (:require [advent-of-code.utils :as u]))

(def input "day16.txt")

(defn transform-input
  [input]
  (->> input
       u/read-file
       u/to-matrix))

(defn valid-coords
  [grid [y x]]
  (and (< -1 y (count grid))
       (< -1 x (count (first grid)))))

(def dir->coords
  {:right [0 1]
   :left  [0 -1]
   :down  [1 0]
   :up    [-1 0]})

(defn get-next-coord
  [[y x] dir]
  (let [next-coord (mapv + (dir->coords dir) [y x])]
    (list next-coord dir)))

(defn find-next-move
  [[y x] dir ch]
  (let [coord [y x]]
    (case ch
      \. (list (get-next-coord coord dir))
      \| (case dir
           (:right :left) (list (get-next-coord coord :up) (get-next-coord coord :down))
           (:up :down) (list (get-next-coord coord dir)))
      \- (case dir
           (:up :down) (list (get-next-coord coord :left) (get-next-coord coord :right))
           (:right :left) (list (get-next-coord coord dir)))
      \\ (list (get-next-coord coord (case dir
                                       :up    :left
                                       :right :down
                                       :down  :right
                                       :left  :up)))
      \/ (list (get-next-coord coord (case dir
                                       :up    :right
                                       :right :up
                                       :down  :left
                                       :left  :down)))
      ())))

(defn energize-tiles
  [start grid]
  (loop [[move & moves] (list start), seen #{}, energized #{}]
    (if (and (nil? move)
             (nil? moves))
      (count energized)
      (let [[coords dir] move
            next-moves (find-next-move coords dir (get-in grid coords))
            next-moves (filter #(valid-coords grid (first %)) next-moves)]
        (if (not (seen move))
          (recur (concat moves next-moves) (conj seen move) (conj energized coords))
          (recur moves seen energized))))))

(defn day-16-part-1
  [input]
  (->> input
       transform-input
       (energize-tiles [[0 0] :right])))

(defn generate-starts
  [grid]
  (letfn [(gen-rows
            [max-x]
            (reduce concat (for [x (range max-x)]
                             (list [[0 x] :down] [[(dec max-x) x] :up]))))
          (gen-cols
            [max-y]
            (reduce concat (for [y (range max-y)]
                             (list [[y 0] :right] [[y (dec max-y)] :left]))))]
    (let [max-y (count grid)
          max-x (count (first grid))]
      (concat (gen-rows max-x) (gen-cols max-y)))))

(defn find-highest-energized
  [grid]
  (pmap #(energize-tiles % grid) (generate-starts grid)))

(defn day-16-part-2
  [input]
  (->> input
       transform-input
       find-highest-energized
       (reduce max)))

(time (day-16-part-1 input))
(time (day-16-part-2 input))
