(ns aoc18.solutions.day3
  (:require [clojure.string :refer [split-lines]])
  (:gen-class))

(defn claim-to-rect
  [claim]
  (let [[id x y w h] (->> (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)
                          (drop 1)
                          (map #(Long. %)))]
    [id x y (+ x w) (+ y h)]))

(defn rect-id [rect] (first rect))
(defn rect-x0 [rect] (nth rect 1))
(defn rect-y0 [rect] (nth rect 2))
(defn rect-x1 [rect] (nth rect 3))
(defn rect-y1 [rect] (nth rect 4))

(defn overlap
  [rect]
  (mapcat
   (fn [y]
     (map #(vector y %) (range (rect-x0 rect) (rect-x1 rect))))
   (range (rect-y0 rect) (rect-y1 rect))))

(defn assign-rects
  [rects]
  (defn add-rect
    [init-fabric rect]
    (reduce
     (fn [fabric pos]
       (update-in fabric pos inc))
     init-fabric
     (overlap rect)))

  (let [fabric (->> (repeat 1000 0)
                    (apply vector)
                    (repeat 1000)
                    (apply vector))]
    (reduce add-rect fabric rects)))

(defn part1
  [^String input]
  (defn count-overlapping
    [fabric]
    (defn count-row
      [row]
      (reduce
       (fn [acc sq]
         (if (> sq 1)
           (inc acc)
           acc))
       0
       row))

    (reduce
     (fn [acc row]
       (+ acc (count-row row)))
     0
     fabric))

  (let [n (->> (map claim-to-rect (split-lines input))
               assign-rects
               count-overlapping)]
    (assert (= n 110383))
    (println n "overlapping square inches")))

(defn part2
  [^String input]
  (defn check-rect
    [fabric rect]
    (reduce
     (fn [unique pos]
       (and (= (get-in fabric pos) 1) unique))
     true
     (overlap rect)))

  (defn find-unique
    [fabric rects]
    (reduce
     (fn [_ rect]
       (if-let [unique (check-rect fabric rect)]
         (reduced (rect-id rect))
         nil))
     nil
     rects))

  (let [rects (map claim-to-rect (split-lines input))
        fabric (assign-rects rects)
        result (find-unique fabric rects)]
    (assert (= result 129))
    (println "Claim" result "not overlapping")))
