(defproject aoc18 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2018"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot aoc18.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
