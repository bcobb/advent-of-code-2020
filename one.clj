(ns one
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn two-sum [numbers sum]
  (first (filter (fn [args] (= sum (apply + args))) (combo/combinations numbers 2))))

(defn three-sum [numbers sum]
  (first (filter (fn [args] (= sum (apply + args))) (combo/combinations numbers 3))))

(defn read-and-parse-input []
  (map #(Integer/parseInt %) (str/split (slurp "resources/one.txt") #"\n")))

(defn first-answer []
  (apply * (two-sum (read-and-parse-input) 2020)))

(defn second-answer []
  (apply * (three-sum (read-and-parse-input) 2020)))
