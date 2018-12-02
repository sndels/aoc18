(ns aoc18.solutions.day2
  (:require [clojure.string :refer [split-lines]])
  (:gen-class))

(defn part1
  [^String input]
  (defn twos_and_threes
    [ids]
    (reduce
     (fn [[twos threes] next]
       (let [freqs (frequencies next)
             has_twos (if (some #(= 2 %) (vals freqs)) 1 0)
             has_threes (if (some #(= 3 %) (vals freqs)) 1 0)]
         [(+ twos has_twos) (+ threes has_threes)]))
     [0 0]
     ids))

  (defn checksum
    [ids]
    (let [[twos threes] (twos_and_threes ids)]
      (* twos threes)))

  (let [ids (map seq (split-lines input))
        result (checksum ids)]
    (assert (= result 6696))
    (println "Checksum is" result)))

(defn part2
  [^String input]
  (defn off_by_one
    [id1 id2]
    (let [matching (filter
                    (fn [[c1 c2]] (= c1 c2))
                    (map vector id1 id2))]
      (if (= (+ (count matching) 1) (count id1))
        (apply str (map (fn [[a b]] a) matching))
        nil)))

  (defn first_correct
    [ids]
    (let [head (first ids)]
      (loop [[next & tail] (rest ids)]
        (if-let [matching (off_by_one head next)]
          matching
          (if next (recur tail) nil)))))

  (defn find_boxes
    [ids]
    (if-let [matching (first_correct ids)]
      matching
      (find_boxes (rest ids))))

  (let [ids (map seq (split-lines input))
        result (find_boxes ids)]
    (assert (= result "bvnfawcnyoeyudzrpgslimtkj"))
    (println "Common letters are" result)))
