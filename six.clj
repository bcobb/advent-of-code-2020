(ns six
  (:require [clojure.string :as str]))

(defn read-form-groups []
  (str/split (slurp "resources/six.txt") #"\n\n"))

(defn yes-answer-count [form-group]
  (count (distinct (remove 
                    (comp (partial re-matches #"\s") str) 
                    form-group))))

(defn unanimous-answer-count [form-group]
  (let [individuals (str/split-lines form-group)
        individual-responses (flatten (mapv (comp (partial into []) seq)
                                            individuals))
        answer-tallies (reduce #(update %1 %2 (fnil inc 0)) {} individual-responses)]
    (count (filter #(= (val %) (count individuals)) answer-tallies))))

(defn first-answer []
  (reduce + (map yes-answer-count (read-form-groups))))

(defn second-answer []
  (reduce + (map unanimous-answer-count (read-form-groups))))
