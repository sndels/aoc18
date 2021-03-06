(ns aoc18.core
  (:require [aoc18.solutions.day1])
  (:require [aoc18.solutions.day2])
  (:require [aoc18.solutions.day3])
  (:require [aoc18.solutions.day4])
  (:require [aoc18.solutions.day5]))

(defn -main
  [& args]
  (if (= (count args) 2)
    (do
      (def day (str "day" (first args)))
      (def part (str "part" (second args)))
      (def input (slurp (str "resources/" day ".txt")))
      (def f (ns-resolve (symbol (str "aoc18.solutions." day)) (symbol part)))
      (f input))
    (println "Usage [day number] [part number]"))
  0)
