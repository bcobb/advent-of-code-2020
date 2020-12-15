(ns thirteen
  (:require [clojure.string :as str]))

(defn read-notes []
  (let [[departure lines-string] (str/split-lines (slurp "resources/thirteen.txt"))
        lines (map-indexed (fn [index line]
                             (if (= "x" line)
                               [index line]
                               [index (Integer/parseInt line)]))
                            (str/split lines-string #","))]
    {:earliest-departure (Integer/parseInt departure)
     :lines lines}))

(defn departures [line]
  (iterate (partial + line) 0))

(defn first-answer []
  (let [{earliest-departure :earliest-departure
         lines :lines} (read-notes) 
        in-service (->> lines
                        (map last)
                        (remove (partial = "x")))
        line-departures (zipmap
                         in-service
                         (map
                          (fn [line]
                            (first (filter #(> % earliest-departure)
                                           (departures line))))
                          in-service))
        [chosen-line chosen-departure] (first (sort-by #(- (val %) earliest-departure) line-departures))]
    (* chosen-line (- chosen-departure earliest-departure))))

(defn second-answer []
  (let [{lines :lines} (read-notes)]
    (loop [queue (rest lines)
           step (last (first lines))
           t (last (first lines))]
      (if (empty? queue)
          t
          (let [[offset divisor] (first queue)]
            (if (= "x" divisor)
              (recur (rest queue)
                     step
                     t)
              (if (zero? (mod (+ t offset) divisor))
                (recur (rest queue)
                       (*' step divisor)
                       t)
                (recur queue
                       step
                       (+' t step)))))))))
