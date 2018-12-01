(ns aoc18.solutions.day1
  (:require [clojure.set :refer [intersection]])
  (:require [clojure.string :refer [split-lines]])
  (:gen-class))

(defn part1
  [^String input]
  (def lines (split-lines input))
  (def freqs (map (fn [s] (Long. (re-find #"[-+]?\d+" s))) lines))
  (def result (reduce + 0 freqs))
  (assert (= result 472))
  (println "Final frequency is" (str result)))

(defn part2
  [^String input]
  (defn apply_freqs
    [init freqs]
    (reduce
     (fn [[freq, seen] next]
       (def new_freq (+ freq next))
       (if (seen new_freq)
         (reduced new_freq)
         [new_freq, (conj seen, new_freq)]))
     init
     freqs))

  (defn first_twice
    [^longs freqs]
    (loop [state [0, #{0}]]
      (def result (apply_freqs state freqs))
      (if (number? result)
        result
        (recur result))))

  (def lines (split-lines input))
  (def freqs (map (fn [s] (Long. (re-find #"[-+]?\d+" s))) lines))
  (def result (first_twice freqs))
  (assert (= result 66932))
  (println "First frequency reached twice is" (str result)))
