(ns eleven
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn read-grid []
  (mapv vec (str/split-lines (slurp "resources/eleven.txt"))))

(defn immediate-neighbor-coordinates [coordinate]
  (let [search-space (combo/cartesian-product [-1 0 1] [-1 0 1])]
    (->> search-space
         (map #(map + coordinate %))
         (remove (partial = coordinate))
         (remove #(some neg? %)))))

(defn grid-coordinates [grid]
  (let [height (count grid)
        width (count (first grid))]
    (combo/cartesian-product (range 0 height) (range 0 width))))

(def empty-seat? #{\L})
(def occupied-seat? #{\#})

(defn visible-neighbors [grid coordinate]
  (reduce
   (fn [neighbors unit]
     (loop [distance 1]
       (let [relative-coordinates (map (partial * distance) unit)
             coordinates (map + coordinate relative-coordinates)
             value (get-in grid coordinates)]
         (if value
           (if (or (empty-seat? value) (occupied-seat? value))
             (cons value neighbors)
             (recur (inc distance)))
           neighbors))))
   '()
   (remove (partial = '(0 0)) (combo/cartesian-product [-1 0 1] [-1 0 1]))))

(defn initial-become-occupied? [grid coordinate]
  (and (empty-seat? (get-in grid coordinate))
       (->> (immediate-neighbor-coordinates coordinate)
            (map (partial get-in grid))
            (filter occupied-seat?)
            (empty?))))

(defn initial-become-empty? [grid coordinate]
  (let [crowded? (fn [occupied] (>= (count occupied) 4))]
    (and (occupied-seat? (get-in grid coordinate))
         (->> (immediate-neighbor-coordinates coordinate)
              (map (partial get-in grid))
              (filter occupied-seat?)
              (crowded?)))))

(defn floor? [grid coordinate]
  (= \. (get-in grid coordinate)))

(defn initial-next-value [grid coordinate]
  (if (floor? grid coordinate)
    \.
    (cond
      (initial-become-empty? grid coordinate) \L
      (initial-become-occupied? grid coordinate) \#
      :else (get-in grid coordinate))))

(defn second-next-value [grid coordinate]
  (if (floor? grid coordinate)
    \.
    (let [seats (visible-neighbors grid coordinate)]
      (cond
        (>= (count (filter occupied-seat? seats)) 5) \L
        (empty? (filter occupied-seat? seats)) \#
        :else (get-in grid coordinate)))))

(defn step 
  ([grid] (step grid initial-next-value))
  ([grid next-value-fn]
   (let [coordinates (grid-coordinates grid)]
     (reduce (fn [newgrid coordinate]
               (assoc-in newgrid coordinate (next-value-fn grid coordinate)))
             grid
             coordinates))))

(defn stabilize 
  ([grid] (stabilize grid initial-next-value))
  ([grid next-value-fn]
   (loop [g grid]
     (let [next (step g next-value-fn)]
       (if (= next g)
         g
         (recur next))))))

(defn first-answer []
  (let [stable-grid (stabilize (read-grid))
        coordinates (grid-coordinates stable-grid)]
    (->> coordinates
         (map (partial get-in stable-grid))
         (filter occupied-seat?)
         (count))))

(defn second-answer []
  (let [stable-grid (stabilize (read-grid) second-next-value)
        coordinates (grid-coordinates stable-grid)]
    (->> coordinates
         (map (partial get-in stable-grid))
         (filter occupied-seat?)
         (count))))
