(ns nine
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn sliding-window 
  ([seq length] (sliding-window seq length 0))
  ([seq length offset] (lazy-seq 
                        (cons (drop offset (take (+ offset length) seq))
                              (sliding-window seq length (inc offset))))))

(defn running-total 
  ([seq] (running-total seq 0))
  ([seq total] (lazy-seq (when (first seq) 
                           (let [next-total (+ total (first seq))] 
                                             (cons next-total (running-total (rest seq) next-total)))))))

(defn ends-with-two-sum? [window]
  (let [sum (last window)
        summands (take (dec (count window)) window)
        sums (map (partial apply +) (combo/combinations summands 2))]
    (some (partial = sum) sums)))

(defn read-input []
  (map #(Integer/parseInt %) (str/split-lines (slurp "resources/nine.txt"))))

(defn first-answer [preamble]
  (let [numbers (read-input)]
    (last (first (filter (comp not ends-with-two-sum?) (sliding-window numbers (inc preamble)))))))

;; take while the sum is less than or equal to the first answer
;; if equal to, done
;; otherwise, move offset up by one and continue
(defn second-answer [preamble]
  (let [target (first-answer preamble)
        numbers (read-input)]
    (loop [offset 0]
      (let [totals (take-while #(<= % target) (running-total (drop offset numbers)))]
        (when (not-empty totals)
          (if (= (last totals) target)
            (let [summands (drop offset (take (+ offset (count totals)) numbers))]
              (+ (apply max summands) (apply min summands)))
            (recur (inc offset))))))))
