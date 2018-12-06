(ns aoc18.solutions.day5
  (:require [clojure.string :refer [upper-case lower-case]])
  (:gen-class))

(defn are-reacting-units
  [a b]
  (defn is-uppercase [c] (= c (get (upper-case c) 0)))
  (defn is-lowercase [c] (= c (get (lower-case c) 0)))

  (if (or (and (is-uppercase a) (is-lowercase b))
          (and (is-lowercase a) (is-uppercase b)))
    (= (lower-case a) (lower-case b))
    false))

(defn react-poly
  [polymer]
  (defn reactor
    [rpoly unit]
    (if (and (first rpoly) (are-reacting-units (first rpoly) unit))
      (rest rpoly)
      (cons unit rpoly)))

  (reverse (reduce reactor '() polymer)))

(defn part1
  [^String input]
  (let [result (->> (drop-last input)
                    seq
                    react-poly
                    count)]
    (assert (= result 11814))
    (println result "units in fully reacted polymer")))

(defn filtered-poly
  [polymer unit]
  (defn filter-unit
    [rpoly next]
    (if (= (get (lower-case next) 0) unit)
      rpoly
      (cons next rpoly)))
  (reverse (reduce filter-unit '() polymer)))

(defn part2
  [^String input]
  (defn find-shortest
    [polymer]

    (defn keep-shortest
      [[length poly] unit]
      (let [new-poly (->> unit
                          (filtered-poly polymer)
                          react-poly)
            new-length (count new-poly)]
        (if (< new-length length)
          [new-length new-poly]
          [length poly])))

    (let [a-to-z (seq "abcdefghijklmnopqrstuvwxyz")]
      (first (reduce keep-shortest [(count polymer) polymer] a-to-z))))

  (let [result (->> (drop-last input)
                    seq
                    find-shortest)]
    (assert (= result 4282))
    (println result "units in shortest fully reacted polymer")))
