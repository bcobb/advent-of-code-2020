(ns two
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[[_ min-c max-c character-s password]] (re-seq #"(\d+)-(\d+) (\w+): (\w+)" line)]
    {:min (Integer/parseInt min-c)
     :max (Integer/parseInt max-c)
     :character (first character-s)
     :password password}))
  
(defn read-lines []
  (str/split (slurp "resources/two.txt") #"\n"))

(defn meets-policy? [line]
  (let [{min :min
         max :max
         character :character
         password :password} (parse-line line)
         occurrences (count (filter (partial = character) (seq password)))]
    (<= min occurrences max)))

(defn meets-new-policy? [line]
  (let [{min :min
         max :max
         character :character
         password :password} (parse-line line)
        policy-indexes [(dec min) (dec max)]
        password->character (partial get password)
        matches-policy-character? (partial = character)]
    (= 1 (count (filter matches-policy-character?
                        (map password->character policy-indexes))))))

(defn first-answer []
  (count (filter meets-policy? (read-lines))))

(defn second-answer []
  (count (filter meets-new-policy? (read-lines))))
