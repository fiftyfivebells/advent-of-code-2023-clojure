(ns advent-of-code.day08
  (:require
   [clojure.string :as str]
   [advent-of-code.utils :as u]))

(defn node->map
  [node-map node]
  (let [[key vals] (str/split node #" = ")]
    (assoc node-map
           key
           (re-seq #"[A-Z0-9]{3}" vals))))

(defn input->network
  [block]
  (->>
   block
   (str/split-lines)
   (reduce node->map {})))

(defn directions-and-network
  [input]
  (let [[dirs network] (u/to-blocks input)]
    {:dirs (seq dirs)
     :network (input->network network)}))

(defn choose-direction
  [ch]
  (if (= ch \R)
    second
    first))

(defn travel
  [{:keys [dirs network]}]
  (loop [[d & ds] (cycle dirs), key "AAA", count 0]
    (if (= key "ZZZ")
      count
      (recur ds ((choose-direction d) (network key)) (inc count)))))

(defn day-8-part-1
  [input]
  (->>
   input
   (directions-and-network)
   travel))

(day-8-part-1 "day08.txt")

(defn least-common-multiple
  [a b]
  (let [gcd (fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
    (cond
      (zero? a) 0
      (zero? b) 0
      :else (* b (/ a (gcd a b))))))

(defn walk-one-path
  [start ends dirs network]
  (loop [[d & ds] (cycle dirs), key start, count 0]
    (if (contains? ends key)
      count
      (recur ds ((choose-direction d) (network key)) (inc count)))))

(defn ghost-travel
  [{:keys [dirs network]}]
  (let [starts (filter #(= \A (last %)) (keys network))
        ends  (set (filter #(= \Z (last %)) (keys network)))]
    (reduce least-common-multiple (map #(walk-one-path % ends dirs network) starts))))

(def result
  (ghost-travel (directions-and-network "day08.txt")))
