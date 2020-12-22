(ns eighteen
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defn read-input-as-lists []
  (map (fn [line]
         (read-string (str "(" line ")")))
       (str/split-lines (slurp "resources/eighteen.txt"))))

(defn compute-list [[x & xs]]
  (reduce (fn [lhs [op rhs]] ((resolve op) lhs rhs)) x (partition 2 xs)))

(defn new-compute-list-inner [lhs [[op rhs] & todo]]
  (case op
    * (* lhs (new-compute-list-inner rhs todo))
    + (new-compute-list-inner (+ lhs rhs) todo)
    lhs))

(defn new-compute-list [[x & xs]]
  (new-compute-list-inner x (partition 2 xs)))

(defn compute-line [line]
  (walk/postwalk #(if (list? %)
                    (compute-list %)
                    %) line))

(defn new-compute-line [line]
  (walk/postwalk #(if (list? %)
                    (new-compute-list %)
                    %) line))

(defn first-answer []
  (->> (read-input-as-lists)
       (map compute-line)
       (apply +)))

(defn second-answer []
  (->> (read-input-as-lists)
       (map new-compute-line)
       (apply +)))
