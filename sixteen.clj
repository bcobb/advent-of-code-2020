(ns sixteen
  (:require [clojure.string :as str]))

(defn to-int [s]
  (Integer/parseInt s))

(defn read-nearby []
  (let [all-lines (str/split-lines (slurp "resources/sixteen.txt"))
        nearby-lines (rest (drop-while #(not (str/includes? % "nearby")) all-lines))]
    (->> nearby-lines
         (map #(str/split % #","))
         (map #(map to-int %)))))

(defn read-my-ticket []
  (let [all-lines (str/split-lines (slurp "resources/sixteen.txt"))
        my-line (first (rest (drop-while #(not (str/includes? % "your ticket")) all-lines)))]
    (as-> my-line $
      (str/split $ #",")
      (map to-int $))))

(defn parse-range-string [range-string]
  (let [[low-boundary high-boundary] (take 2 (map to-int (str/split range-string #"-")))]
    (range low-boundary (inc high-boundary))))

(defn read-rules []
  (let [all-lines (str/split-lines (slurp "resources/sixteen.txt"))
        rule-lines (filter #(str/includes? % " or ") all-lines)]
    (->> rule-lines
         (map #(re-seq #"([^:]+): ([^ ]+) or ([^ ]+)" %))
         (map (comp rest first))
         (map (fn [[field low-range high-range]] [field (parse-range-string low-range) (parse-range-string high-range)]))
         (reduce #(assoc %1 (first %2) (map set (rest %2))) {}))))

(defn satisfies-rule? [field rule]
  (some (fn [rule-range] (contains? rule-range field)) (val rule)))

(defn valid-ticket? [rules ticket]
  (every? (fn [field]
            (some (partial satisfies-rule? field) rules)) ticket))

(defn columns [matrix]
  (map (fn [column]
         (map #(nth % column) matrix))
       (range 0 (count (first matrix)))))

(defn first-answer []
  (let [rules (read-rules)
        nearby-tickets (read-nearby)
        invalid-fields (map (fn [fields]
                              (filter #(not-any? (partial satisfies-rule? %) rules) fields))
                            nearby-tickets)]
    (reduce + (flatten invalid-fields))))

(defn deduce-fields []
  (let [rules (read-rules)
        nearby-tickets (read-nearby)
        valid-tickets (filter (partial valid-ticket? rules) nearby-tickets)
        positional-candidates (->> (columns valid-tickets)
                                   (map (fn [field-values]
                                          (->> rules
                                               (filter (fn [rule]
                                                         (every? #(satisfies-rule? % rule) field-values)))
                                               (map first))))
                                   (map-indexed #(vector %1 %2))
                                   (sort-by (comp count last)))
        deduced-layout (reduce
                        (fn [{fields :fields
                              columns :column :as hm} [column candidates]]
                          (let [known (set fields)
                                unknown (remove known candidates)]
                            (-> hm
                                (update :columns conj column)
                                (update :fields conj (first unknown)))))
                        {:fields [] :columns []}
                        positional-candidates)]
    (zipmap (:fields deduced-layout) (:columns deduced-layout))))

(defn second-answer []
  (->> (deduce-fields)
       (filter #(str/includes? (key %) "departure"))
       (map last)
       (map #(nth (read-my-ticket) %))
       (reduce *)))
