(ns seventeen
  (:require [clojure.string :as str]
            [clojure.set :as cset]
            [clojure.math.combinatorics :as combo]))

(defn read-input [base]
  (set (apply concat (map-indexed (fn [row line]
                                    (map last
                                         (filter (comp (partial = \#) first)
                                                 (map-indexed (fn [column character] [character (-> base
                                                                                                    (assoc 0 row)
                                                                                                    (assoc 1 column))])
                                                              (vec line)))))
                                  (str/split-lines (slurp "resources/seventeen.txt"))))))

(defn relative-neighbor-coordinates [dimensions]
  (map vec (remove (partial every? zero?) (apply combo/cartesian-product (take dimensions (repeat [0 1 -1]))))))

(defn neighbor-coordinates [coordinate]
  (set (map (fn [relative-neighbor] (mapv + coordinate relative-neighbor)) (relative-neighbor-coordinates (count coordinate)))))

(defn searchable-coordinates [state]
  (apply cset/union (map neighbor-coordinates state)))

(defn lives? [state coordinate]
  (let [active-neighbors (count (filter
                                 (partial contains? state)
                                 (neighbor-coordinates coordinate)))
        active? (contains? state coordinate)]
    (if active?
      (<= 2 active-neighbors 3)
      (= 3 active-neighbors))))

(defn run-cycle [state]
  (set (filter (partial lives? state)
               (searchable-coordinates state))))

(defn first-answer []
  (count (last (take 7 (iterate run-cycle (read-input [0 0 0]))))))

(defn second-answer []
  (count (last (take 7 (iterate run-cycle (read-input [0 0 0 0]))))))
