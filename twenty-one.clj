(ns twenty-one
  (:require [clojure.string :as str]
            [clojure.set :as cset]))

(defn parse-foods []
  (->> (str/split-lines (slurp "resources/twenty-one.txt"))
       (map #(re-matches #"([^\)]+) \(contains (.+)\)" %))
       (map rest)
       (map (fn [[ingredients allergens]]
              {:ingredients (set (str/split ingredients #" "))
               :allergens (set (str/split allergens #", "))}))))

(defn deduce-responsible-ingredients [foods]
  (let [allergens (apply cset/union (map :allergens foods))
        allergens-to-ingredients (reduce
                                  (fn [m allergen]
                                    (let [associated-foods (filter #(contains? (:allergens %) allergen) foods)
                                          common-ingredients (apply cset/intersection (map :ingredients associated-foods))]
                                      (assoc m allergen common-ingredients)))
                                  {}
                                  allergens)]
    (loop [possibilities allergens-to-ingredients]
      (let [unknown-degree (->> (vals possibilities)
                                (map count)
                                flatten
                                (apply max))]
        (if (> unknown-degree 1)
          (let [knowns (->> (vals possibilities)
                            (filter (comp (partial = 1) count))
                            (apply cset/union))]
            (recur (into {} (map (fn [me]
                                   (vector
                                    (key me)
                                    (if (> (count (val me)) 1)
                                      (cset/difference (val me) knowns)
                                      (val me))))
                                 possibilities))))
          possibilities)))))

(defn first-answer []
  (let [foods (parse-foods)
        ingredients (apply cset/union (map :ingredients foods))
        ingredient-appearances (flatten (map (comp seq :ingredients) foods))
        deductions (deduce-responsible-ingredients foods)
        non-allergic-ingredients (cset/difference ingredients (apply cset/union (vals deductions)))]
    (count (filter non-allergic-ingredients ingredient-appearances))))

(defn second-answer []
  (let [foods (parse-foods)
        deductions (deduce-responsible-ingredients foods)]
    (->> deductions
         vec
         (sort-by first)
         (map (comp first last))
         (str/join ","))))
