(ns twenty-five)

(defn loop-once [subject-number value]
  (let [value' (* value subject-number)]
    (rem value' 20201227)))

(defn transform [subject-number times]
  (first (drop times (iterate (partial loop-once subject-number) 1))))

(defn determine-loop-size [subject-number value public-key]
  (first (keep-indexed (fn [index loop-value] (when (= public-key loop-value) index))
                       (iterate (partial loop-once subject-number) value))))

(defn first-answer [card-public-key door-public-key]
  (let [card-loop-size (determine-loop-size 7 1 card-public-key)
        door-loop-size (determine-loop-size 7 1 door-public-key)]
    (assert (= (transform card-public-key door-loop-size) (transform door-public-key card-loop-size)) "encryption keys not equal")
    (transform card-public-key door-loop-size)))
