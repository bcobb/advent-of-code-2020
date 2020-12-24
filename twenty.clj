(ns twenty
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn read-inputs []
  (map (fn [[title-line & tile-lines]] [title-line (mapv vec tile-lines)]) (map str/split-lines
                                                                                (str/split (slurp "resources/twenty.txt") #"\n\n"))))

(defn transpose [m]
  (apply mapv vector m))

(defn rotate-clockwise [m]
  (-> m reverse vec transpose))

(defn flip-vertical [m]
  (vec (reverse m)))

(defn top-row [m]
  (first m))

(defn left-column [m]
  (mapv first m))

(defn right-column [m]
  (mapv last m))

(defn bottom-row [m]
  (last m))

(defn matrix-variations [m]
  (distinct (reduce (fn [variations m*] (-> variations
                                            (conj m*)
                                            (conj (flip-vertical m*))))
                    []
                    (take 4 (iterate rotate-clockwise m)))))

(defn could-be-neighbors? [m1 m2]
  (let [adjacencies {'top-row 'bottom-row
                     'right-column 'left-column}
        m1s (matrix-variations m1)
        m2s (matrix-variations m2)]
    (some (fn [[m1* m2*]]
            (some (fn [me]
                    (let [side-a (resolve (key me))
                          side-b (resolve (val me))]
                      (or (= (side-a m1*) (side-b m2*))
                          (= (side-b m1*) (side-a m2*)))))
                  adjacencies))
          (combo/cartesian-product m1s m2s))))

(defn orient [direction m1 m2]
  (let [slices {:right ['right-column 'left-column]
                :left ['left-column 'right-column]
                :up ['top-row 'bottom-row]
                :down ['bottom-row 'top-row]}
        [m1-slice m2-slice] (map resolve (get slices direction))
        m2s (matrix-variations m2)]
    (filter (fn [m2*]
              (= (m1-slice m1) (m2-slice m2*)))
            m2s)))

(defn determine-neighbors [ids index]
  (let [id-pairs (remove (partial apply =) (combo/cartesian-product ids ids))]
    (filter (fn [[id1 id2]]
              (could-be-neighbors? (get index id1) (get index id2)))
            id-pairs)))

(defn blank-matrix [l]
  (mapv (fn [_] (vec (repeat l nil))) (repeat l nil)))

(defn first-answer []
  (let [inputs (read-inputs)
        ids (map (comp #(Integer/parseInt %) ffirst (partial re-seq #"(\d+)") first) inputs)
        images (map last inputs)
        index (zipmap ids images)
        neighbor-pairs (determine-neighbors ids index)
        neighbors-by-id (group-by first neighbor-pairs)
        corners (filter #(= 2 (count (val %))) neighbors-by-id)]
    (assert (= 4 (count corners)) "not 4 corners")
    (apply * (map first corners))))


;; lmfao
;; take a corner piece, and generate every transformation of that piece
;; for every transformation of the piece, start with an image where that corner is in the top left of the final image
;; then go cell by cell, left to right, top to bottom, finding which of the available pieces fits
;; if we're in the middle of a row, the next piece's left border must fit the previous piece's right border
;; if we're at the beginning of a row, the next piece's top border must fit the first piece in the previous row's bottom border
(defn build-image []
  (let [inputs (read-inputs)
        ids (map (comp #(Integer/parseInt %) ffirst (partial re-seq #"(\d+)") first) inputs)
        images (map last inputs)
        index (zipmap ids images)
        neighbor-pairs (determine-neighbors ids index)
        neighbors-by-id (into {} (map (fn [me] [(key me) (remove (partial = (key me)) (flatten (val me)))]) (group-by first neighbor-pairs)))
        corner-ids (map first (filter #(= 2 (count (val %))) neighbors-by-id))
        final-image-dimension (int (Math/sqrt (count images)))
        final-image (blank-matrix final-image-dimension)
        first-corner-id (first corner-ids)
        first-corner (index first-corner-id)]
    (first (filter (fn [attempt] (every? (fn [row] (not-any? nil? row)) attempt))
                   (map (fn [first-corner-variation]
                          (reduce (fn [final-image-rows image-row-number]
                                    (assoc final-image-rows
                                           image-row-number
                                           (reduce (fn [final-image-column image-column-number]
                                                     (let [placed (into (set (map :id final-image-column)) (set (flatten (map #(map :id %) final-image-rows))))
                                                           unplaced (remove placed ids)]
                                                       (if (empty? placed)
                                                         (assoc
                                                          final-image-column
                                                          image-column-number
                                                          {:id first-corner-id
                                                           :tile first-corner-variation})
                                                         (assoc
                                                          final-image-column
                                                          image-column-number
                                                          (if-let [prev (peek final-image-column)]

                                                            (let [matching (keep (fn [[tile-id tile]]
                                                                                   (when-let [variation (first (orient :right (:tile prev) tile))]
                                                                                     {:id tile-id
                                                                                      :tile variation}))
                                                                                 (map vector unplaced (map index unplaced)))]
                                                              (first matching))
                                                            (when-let [prev (get-in final-image-rows [(dec image-row-number) image-column-number])]

                                                              (let [matching (keep (fn [[tile-id tile]]
                                                                                     (when-let [variation (first (orient :down (:tile prev) tile))]
                                                                                       {:id tile-id
                                                                                        :tile variation}))
                                                                                   (map vector unplaced (map index unplaced)))]
                                                                (first matching))))))))
                                                   []
                                                   (range 0 (count final-image)))))
                                  []
                                  (range 0 (count final-image))))
                        (matrix-variations first-corner))))))

(defn middle [coll]
  (rest (drop-last coll)))

(defn without-tile-borders [image]
  (mapv (fn [row]
          (mapv (fn [cell]
                  (update cell :tile (fn [cell] (vec (middle (map (comp vec middle) cell)))))) row)) image))

(def sample-image (build-image))

(defn assemble-tiles [image]
  (vec (map vec (mapcat (fn [row]
                          (let [row-tiles (map :tile row)]
                            (apply map concat row-tiles)))
                        image))))

(defn to-ascii [rows-of-characters]
  (->> rows-of-characters
       (map (partial apply str))
       (str/join "\n")))

(def sea-monster "                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ")

(def sea-monster-cells (->> (str/split-lines sea-monster)
                            (mapv vec)))

(defn cells [height width m]
  (mapcat (fn [starting-row]
            (let [ending-row (+ starting-row height)
                  row-slice (subvec m starting-row ending-row)]
              (map (fn [starting-column]
                     (let [ending-column (+ starting-column width)]
                       (mapv #(subvec % starting-column ending-column) row-slice)))
                   (range 0 (inc (- (count m) width))))))
          (range 0 (inc (- (count m) height)))))

(defn cells-match? [base candidate]
  (= '(true) (distinct (flatten (map (fn [base-row candidate-row]
                                       (distinct (map (fn [base-point candidate-point]
                                                        (if (= \# candidate-point)
                                                          (= \# base-point)
                                                          true)) base-row candidate-row))) base candidate)))))

(defn second-answer []
  (let [found-monsters
        (->> (assemble-tiles (without-tile-borders sample-image))
             (matrix-variations)
             (map (fn [m] (let [monster-cells (cells (count sea-monster-cells) (count (first sea-monster-cells)) m)]
                            (count (filter #(cells-match? % sea-monster-cells) monster-cells)))))
             (reduce +))
        wave-cells (count (filter (partial = \#) (to-ascii (without-tile-borders sample-image))))
        monster-size (count (filter (partial = \#) sea-monster))]
    (- wave-cells (* found-monsters monster-size))))
