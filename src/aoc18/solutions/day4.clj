(ns aoc18.solutions.day4
  (:require [clojure.string :refer [split-lines]])
  (:gen-class))

(defn parse-naps
  [records]
  (defn get-id
    [record]
    (if-let [id (second (re-find #"#(\d+)" record))]
      (Long. id)))

  (defn get-minutes
    [record]
    (if-let [minutes (second (re-find #":(\d+)" record))]
      (Long. minutes)))

  (defn add-nap
    [id start end nap-log]
    (assoc nap-log id (conj (get nap-log id) [start end])))

  (defn iter
    [id asleep-since nap-log remaining]
    (if-let [record (first remaining)]
      (if-let [new-id (get-id record)]
        (recur new-id nil nap-log (rest remaining))
        (if asleep-since
          (recur
           id
           nil
           (add-nap id asleep-since (get-minutes record) nap-log)
           (rest remaining))
          (recur id (get-minutes record) nap-log (rest remaining))))
      nap-log))

  (iter nil nil {} records))

(defn target-minute
  [guard]
  (defn sleep-freq
    [guard]
    (reduce
     (fn [acc-total next-nap]
       (reduce
        (fn [acc-nap next-minute]
          (update-in acc-nap [next-minute] inc))
        acc-total
        (range (first next-nap) (second next-nap))))
     (apply vector (repeat 60 0))
     (second guard)))

  (apply max-key second (map-indexed vector (sleep-freq guard))))

(defn find-sleepiest
  [condition nap-log]
  (apply max-key condition (seq nap-log)))

(defn part1
  [^String input]
  (defn total-naptime
    [guard]
    (reduce
     (fn [acc nap] (+ acc (- (second nap) (first nap))))
     0
     (second guard)))

  (let [guard (->> (split-lines input)
                   sort
                   parse-naps
                   (find-sleepiest total-naptime))
        id (first guard)
        minute (first (target-minute guard))
        result (* (first guard) minute)]
    (assert (= result 84834))
    (println "ID" id "times minute" minute "is" result)))

(defn part2
  [^String input]
  (let [guard (->> (split-lines input)
                   sort
                   parse-naps
                   (find-sleepiest #(second (target-minute %))))
        id (first guard)
        minute (first (target-minute guard))
        result (* (first guard) minute)]
    (assert (= result 53427))
    (println "ID" id "times minute" minute "is" result)))
