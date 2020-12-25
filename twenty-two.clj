(ns twenty-two
  (:require [clojure.string :as str]))

(defn parse-decks []
  (let [[_ a _ b] (partition-by (partial re-seq #"Player") (str/split-lines (slurp "resources/twenty-two.txt")))]
    [(mapv #(Integer/parseInt %) (remove empty? a))
     (mapv #(Integer/parseInt %) (remove empty? b))]))

(defn play-round [deck-a deck-b]
  (cond
    (empty? deck-a) deck-b
    (empty? deck-b) deck-a
    :else
    (let [card-a (first deck-a)
          card-b (first deck-b)
          next-deck-a (subvec deck-a 1)
          next-deck-b (subvec deck-b 1)]
      (if (< card-a card-b)
        (recur next-deck-a (conj next-deck-b card-b card-a))
        (recur (conj next-deck-a card-a card-b) next-deck-b)))))

(defn score [deck]
  (let [scores (reverse (range 1 (inc (count deck))))]
    (apply + (map * deck scores))))

(defn first-answer []
  (score (apply play-round (parse-decks))))

(defn play-recursive [initial-deck-a initial-deck-b]
  (loop [deck-a initial-deck-a
         deck-b initial-deck-b
         history #{}]
    (cond
      (contains? history [deck-a deck-b]) {:winner :a :deck deck-a}
      (empty? deck-a) {:winner :b :deck deck-b}
      (empty? deck-b) {:winner :a :deck deck-a}
      :else
      (let [card-a (first deck-a)
            card-b (first deck-b)
            next-deck-a (subvec deck-a 1)
            next-deck-b (subvec deck-b 1)
            recursive-deck-a (subvec deck-a 1 (min (+ 1 card-a) (count deck-a)))
            recursive-deck-b (subvec deck-b 1 (min (+ 1 card-b) (count deck-b)))
            next-history (conj history [deck-a deck-b])]
        (if (and (<= card-a (count next-deck-a))
                 (<= card-b (count next-deck-b)))
          (case (:winner (play-recursive recursive-deck-a recursive-deck-b))
            :a (recur (conj next-deck-a card-a card-b) next-deck-b next-history)
            :b (recur next-deck-a (conj next-deck-b card-b card-a) next-history))
          (if (< card-a card-b)
            (recur next-deck-a (conj next-deck-b card-b card-a) next-history)
            (recur (conj next-deck-a card-a card-b) next-deck-b next-history)))))))

(defn second-answer []
  (score (:deck (apply play-recursive (parse-decks)))))
