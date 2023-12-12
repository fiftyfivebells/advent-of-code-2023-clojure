(ns advent-of-code.day05
  (:require
   [clojure.string :as str]
   [advent-of-code.utils :as u]))

(def input "day05.txt")

(defn gen-seeds-and-almanac
  [input]
  (let [blocks  (u/to-blocks input)
        seeds   (map parse-long (re-seq #"\d+" (first blocks)))
        almanac (rest blocks)]
    [seeds almanac]))

(defn los->lol
  "Takes a list of strings and turns it into a list of longs."
  [los]
  (map parse-long (re-seq #"\d+" los)))

(defn almanac-page->lists
  "Takes a 'page' from the almanac and converts it to a list of longs."
  [page]
  (map los->lol page))

(defn almanac-map->lists
  "Maps the entire almanac to a list of lists of longs. These are in a specific order
  so there's no need to make this a map and give keywords to each page (although that
  would probably make it clearer to read in at a glance)."
  [almanac]
  (->>
   almanac
   (map #(str/split-lines %))
   (map rest)
   (map almanac-page->lists)))

(defn seed-in-line?
  "Takes a seed value and a line in an almanac page, and checks to see if the seed value
  is contained in the range defined by this line."
  [seed line]
  (let [[_ src-start step] line]
    (<= src-start
        seed
        (+ src-start (dec step)))))

(defn get-seed-from-line
  [seed line]
  (let [[dest-start src-start _] line]
    (+ dest-start (- seed src-start))))

(defn get-seed-from-map
  "Takes in a seed and seed->destination mapping (almanac page) and gets the
  corresponding value from the mapping. It checks to see if the almanac defines a
  non-standard mapping and uses that if it does. Otherwise, it just returns the seed
  value as-is."
  [seed dest-map]
  (loop [[x & xs] dest-map]
    (cond
      (nil? x) seed
      (seed-in-line? seed x) (get-seed-from-line seed x)
      :else (recur xs))))

(defn source->destination
  "Given a source, it gets the destination mapping from the almanac. For example,
  if given a seed value, it looks it up in the seed->soil almanac and gives back the
  corresponding value (which can be used as a seed for the next almanac)."
  [source almanac-map]
  (map #(get-seed-from-map % almanac-map) source))

(defn seed->location
  "Takes a seed and maps it through the whole almanac to a location."
  [seed almanac]
  (let [[seed->soil soil->fert fert->water water->light light->temp temp->humid humid->loc] (almanac-map->lists almanac)
        soil  (get-seed-from-map seed seed->soil)
        fert  (get-seed-from-map soil soil->fert)
        water (get-seed-from-map fert fert->water)
        light (get-seed-from-map water water->light)
        temp  (get-seed-from-map light light->temp)
        humid (get-seed-from-map temp temp->humid)]
    (get-seed-from-map humid humid->loc)))

(defn day-5-part-1
  [input]
  (let [[seeds almanac] (gen-seeds-and-almanac input)]
    (reduce min (map #(seed->location % almanac) seeds))))

;; Part 2
;;
;; This was a bit over my head. All the solutions I came up with on my own took too
;; long to run and overflowed the heap space. I looked at some other people's solutions
;; (no clojure solutions though!) and adapted the algorithms to clojure. Even as is, it
;; still takes ~10 seconds or so to calculate. These are big numbers!

(defn range<
  "This is a helper function to compare the minimum of two ranges. Each vector argument
  is a pair of (result, seed), and this just compares them to see which of the two is
  the smaller value."
  [[r1f r1s] [r2f r2s]]
  (if (not= r1f r2f)
    (< r1f r2f)
    (< r2s r2s)))

(defn get-range-min
  "Helper function used in a reduce call. This takes in a current min range and a new
  value to compare. Returns the new range if it's smaller, or the current one otherwise."
  [mins range-min]
  (if (or (= -1 (first mins))
          (range< range-min mins))
    range-min
    mins))

(defn get-smallest-in-range
  "Takes a range and a step value, then steps through the range in step-value
  increments. For each iteration, it calculates the location from the seed and then
  compares it to the current minimum. Returns the smallest value in the range."
  [rng step almanac]
  (loop [[x & xs] (range (first rng) (apply + rng) step), min [-1 -1]]
    (if (nil? x)
      min
      (let [val     (seed->location x almanac)
            new-min (get-range-min min [val x])]
        (recur xs new-min)))))

(defn get-smallest-all-ranges
  "Takes the list of seeds, partitions it into pairs (ranges), iterates through the list
  of seed ranges and then attempts to find the min in each range by stepping through the
  range in huge increments (to start). The increments get smaller and the seed ranges
  get pared down each step by taking the smallest seed/value pair in the range. Finally,
  when the step is small enough, it calls the get-smallest-in-range function with the
  smallest range pair and a step of 1. It then tries every value in the enormously
  limited range pool until it finds the actual smallest value."
  [seeds almanac]
  (let [all-ranges (partition 2 seeds)
        stop       10000]
    (loop [step 100000000, min [-1 -1], all-ranges all-ranges]
      (if (< step stop)
        (let [last-range (list (- (first (first all-ranges)) step) (* step 10))]
          (first (get-smallest-in-range last-range 1 almanac)))
        (let [range-mins  (map #(get-smallest-in-range % step almanac) all-ranges)
              range-min   (reduce get-range-min min range-mins)
              seed        (list (- (last range-min) step) (* step 10))
              seed-ranges (list seed)]
          (recur (/ step 10) range-min seed-ranges))))))

(defn day-5-part-2
  [input]
  (let [[seeds almanac] (gen-seeds-and-almanac input)]
    (get-smallest-all-ranges seeds almanac)))

(day-5-part-2 input)
(day-5-part-1 input)
