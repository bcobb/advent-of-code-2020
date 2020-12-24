(ns nineteen
  (:require [clojure.string :as str]))

(defn parse-rule-line [rule-line]
  (let [[id description] (str/split rule-line #":")]
    [(Integer/parseInt id)
     (if-let [letter (re-find #"[ab]" description)]
       letter
       (read-string (str "(" description ")")))]))

(defn flatten-rules [rulebook rule-id max-traversals]
  (if (zero? max-traversals)
    ""
    (if (list? (get rulebook rule-id))
      (let [terms (get rulebook rule-id)
            flattened-terms (map (fn [term] (if (number? term)
                                              (let [traversal (if (= term rule-id) (dec max-traversals) max-traversals)]
                                                (flatten-rules rulebook term traversal))
                                              term))
                                 terms)]
        (str "(" (apply str flattened-terms) ")"))
      (get rulebook rule-id))))

(defn first-answer []
  (let [[rules-block messages-block] (str/split (slurp "resources/nineteen.txt") #"\n\n")
        messages (str/split-lines messages-block)
        rulebook (->> (str/split-lines rules-block)
                      (map parse-rule-line)
                      (into {}))
        pattern (re-pattern (flatten-rules rulebook 0 1))]
    (count (filter #(re-matches pattern %) messages))))

(defn second-answer []
  (let [[rules-block messages-block] (str/split (slurp "resources/nineteen.txt") #"\n\n")
        messages (str/split-lines messages-block)
        rulebook (->> (str/split-lines rules-block)
                      (map parse-rule-line)
                      (into {}))
        new-rulebook (-> rulebook
                         (assoc 8 '(42 | 42 8))
                         (assoc 11 '(42 31 | 42 11 31)))
        pattern (re-pattern (flatten-rules new-rulebook 0 5))]
    (count (filter #(re-matches pattern %) messages))))
