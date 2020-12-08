(ns eight
  (:require [clojure.string :as str]))

(defn read-program []
  (let [instructions (->> (str/split-lines (slurp "resources/eight.txt"))
                          (map (fn [line] (str/split line #" ")))
                          (map (fn [[instruction number-string]] [instruction (Integer/parseInt number-string)]))
                          vec)]
    {:instructions instructions
     :position 0
     :accumulator 0}))

(defn jmp [program number]
  (update program :position (partial + number)))

(defn acc [program number]
  (-> program
      (update :accumulator (partial + number))
      (jmp 1)))

(defn nop [program _]
  (update program :position inc))

(defn execute-once [program]
  (let [position (:position program)
        instructions (:instructions program)
        [instruction number] (nth instructions position)]
    (case instruction
      "acc" (acc program number)
      "nop" (nop program number)
      "jmp" (jmp program number))))

(defn execute-program [program]
  (loop [program program
         visited #{(:position program)}]
    (let [instructions (:instructions program)
          next-program (execute-once program)
          next-position (:position next-program)]
      (if (visited next-position)
        (assoc program :terminated false)
        (if-not (get instructions next-position)
          (assoc next-program :terminated true)
          (recur next-program (conj visited next-position)))))))

(defn changesets [program]
  (->> (map-indexed #(vector %1 %2) (:instructions program))
       (filter (fn [[_ [instruction _]]]
                 (or (= instruction "jmp")
                     (= instruction "nop"))))
       (map (fn [[index [instruction number]]]
              (case instruction
                "jmp" [index ["nop" number]]
                "nop" [index ["jmp" number]])))))

(defn apply-changeset [program changeset]
  (let [instructions (:instructions program)]
    (assoc program :instructions (apply assoc instructions changeset))))

(defn first-answer []
  (:accumulator (execute-program (read-program))))

(defn second-answer []
  (let [program (read-program)
        changesets (changesets program)]
    (->> changesets
         (map (partial apply-changeset program))
         (map execute-program)
         (filter :terminated)
         (map :accumulator)
         first)))
