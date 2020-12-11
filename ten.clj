(ns ten
  (:require [clojure.string :as str]))

(defn read-adapters []
  (->> (slurp "resources/ten.txt")
       (str/split-lines)
       (map #(Integer/parseInt %))))

(defn built-in [adapters]
  (+ 3 (apply max adapters)))

(defn all-jolts [adapters]
  (-> adapters
      (conj 0)
      (conj (built-in adapters))
      sort
      vec))

(defn first-answer []
  (let [all (all-jolts (read-adapters))
        differences (map - (rest all) all)
        difference-frequencies (vals (frequencies differences))]
    (apply * difference-frequencies)))

(defn second-answer []
  (let [all (all-jolts (read-adapters))]
    (val 
     (first 
      (reduce
       (fn [paths jolt]
         (let [jolt-paths (->> (map + (repeat jolt) [1 2 3])
                               (keep paths)
                               (apply +))]
           (assoc paths jolt jolt-paths)))
       (sorted-map (last all) 1)
       (rest (reverse all)))))))
