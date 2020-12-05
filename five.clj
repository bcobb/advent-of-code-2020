(ns five
  (:require [clojure.string :as str]))

(defn read-boarding-passes []
  (str/split-lines (slurp "resources/five.txt")))

(def lower? #{\F \L})

(defn search [[lower upper] character]
  (let [midpoint (/ (- upper lower) 2)]
    (if (lower? character)
      [lower (int (- upper midpoint))]
      [(int (Math/ceil (+ lower midpoint))) upper])))

(defn seat-id [pass]
  (let [[_ row-pass column-pass] (re-matches #"([FB]{7})([RL]{3})" pass)
        row (apply min (reduce search [0 127] row-pass))
        column (apply min (reduce search [0 7] column-pass))]
    (+ (* row 8) column)))

(defn first-answer []
  (apply max (map seat-id (read-boarding-passes))))

(defn second-answer []
  (let [seat-ids (sort (map seat-id (read-boarding-passes)))]
    (loop [current (first seat-ids)
           remaining (rest seat-ids)]
      (let [next (first remaining)]
        (if (= 2 (- next current))
          (dec next)
          (recur next (rest remaining)))))))

(defn has-size? [n]
  (fn [coll] (= n (count coll))))

;; if every seat was filled, every partition would be of size one
;; look for the one which has two elements, and the missing seat is the number in the middle of the two
(defn alternate-second-answer []
  (let [sorted-seat-ids (sort (map seat-id (read-boarding-passes)))
        seats-in-divisible-chunks (partition-by odd? sorted-seat-ids)
        [lower-neighbor _] (first (filter (has-size? 2) seats-in-divisible-chunks))]
    (inc lower-neighbor)))
