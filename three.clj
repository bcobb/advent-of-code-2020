(ns three
  (:require [clojure.string :as str]))

(def TREE \#)

(defn read-and-parse-input []
  (mapv (partial apply vector) (str/split-lines (slurp "resources/three.txt"))))

(defn path-coordinates [rows row-step column-step]
  (filter (fn [[row-number _]]
            (< row-number (count rows)))
          (map-indexed (fn [i row] 
                         (let [row-number (inc i)
                               path-row (* row-number row-step)
                               path-column (-> row-number
                                                 (* column-step)
                                                 (mod (count row)))]
                           [path-row path-column]))
                       rows)))

(defn trees-count [rows row-step column-step]
  (let [coordinates (path-coordinates rows row-step column-step)]
    (->> coordinates
         (map (partial get-in rows))
         (filter (partial = TREE))
         (count))))

(defn first-solution []
  (trees-count (read-and-parse-input) 1 3))

(defn second-solution []
  (let [hypotheticals [[1 1]
                       [1 3]
                       [1 5]
                       [1 7]
                       [2 1]]]
    (apply * (map (partial apply trees-count (read-and-parse-input)) hypotheticals))))
