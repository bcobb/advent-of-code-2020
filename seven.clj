(ns seven
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-inputs []
  (str/split-lines (slurp "resources/seven.txt")))

(defn list->contents [[number-string color]]
  {:number (Integer/parseInt number-string)
   :color color})

(defn parse-rule [line]
  (let [[container-string content-string] (str/split line #" contain ")
        container (first (rest (first (re-seq #"(.+) bags" container-string))))
        contents (when-not (= "no other bags." content-string) 
                   (map (comp list->contents rest first (partial re-seq #"(\d) (.+) bag"))
                        (str/split content-string #", ")))]
    [container contents]))

(defn parse-ruleset [lines]
  (reduce (partial apply assoc)
          {}
          (map parse-rule lines)))

(defn colors-held-by [ruleset color]
  (when-let [holdings (set (map :color (get ruleset color)))]
    (reduce set/union holdings (map (partial colors-held-by ruleset) holdings))))

(defn bags-held-by [ruleset color]
  (when-let [contents (get ruleset color)]
    (apply + (map
              (fn [{number :number held-color :color}]
                (+ number
                   (* number (or (bags-held-by ruleset held-color) 0))))
              contents))))

(defn first-answer []
  (count (filter (fn [s] (s "shiny gold"))
                 (map (partial colors-held-by (parse-ruleset (read-inputs))) 
                      (keys (parse-ruleset (read-inputs)))))))

(defn second-answer []
  (bags-held-by (parse-ruleset (read-inputs)) "shiny gold"))
