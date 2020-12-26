(ns twenty-four
  (:require [clojure.string :as str]
            [clojure.set :as cset]))

(def xforms-by-direction {"e" [0 2]
                          "w" [0 -2]
                          "se" [1 1]
                          "sw" [1 -1]
                          "ne" [-1 1]
                          "nw" [-1 -1]})

(defn destination [line]
  (apply map + (map (comp xforms-by-direction first) (re-seq #"(e|w|se|sw|ne|nw)" line))))

(defn first-answer [input-file]
  (count
   (filter
    (comp odd? val)
    (frequencies (map destination (str/split-lines (slurp input-file)))))))

(defn neighbors [cell]
  (set (map (partial map + cell) (vals xforms-by-direction))))

(defn initial-black-tiles [input-file]
  (set (map first (filter (comp odd? val)
                          (frequencies (map destination (str/split-lines (slurp input-file))))))))

(defn flips-to-black? [black-tiles white-cell]
  (let [neighbor-count (count (cset/intersection (neighbors white-cell) black-tiles))]
    (= 2 neighbor-count)))

(defn flips-to-white? [black-tiles black-cell]
  (let [neighbor-count (count (cset/intersection (neighbors black-cell) black-tiles))]
    (or (zero? neighbor-count)
        (> neighbor-count 2))))

(defn apply-rules [black-tiles]
  (let [all-tiles (apply cset/union (map neighbors black-tiles))
        white-tiles (cset/difference all-tiles black-tiles)
        flip-to-black (cset/select (partial flips-to-black? black-tiles) white-tiles)
        flip-to-white (cset/select (partial flips-to-white? black-tiles) black-tiles)]
    (-> black-tiles
        (cset/union flip-to-black)
        (cset/difference flip-to-white))))

(defn second-answer [input-file]
  (loop [i 1
         black-tiles (initial-black-tiles input-file)]
    (if (> i 100)
      (count black-tiles)
      (recur (inc i) (apply-rules black-tiles)))))
