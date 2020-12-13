(ns twelve
  (:require [clojure.string :as str]))

(defn read-directions []
  (->> (slurp "resources/twelve.txt")
       (str/split-lines)
       (map (comp rest first (partial re-seq #"([NSEWLRF])(\d+)")))
       (map (fn [[action amount]] [action (Integer/parseInt amount)]))))

(def left-turns {"E" "N"
                 "N" "W"
                 "W" "S"
                 "S" "E"})

(def right-turns {"E" "S"
                  "S" "W"
                  "W" "N"
                  "N" "E"})

(defn turn [state mapping degrees]
  (let [turns (mod (/ degrees 90) 4)]
    (last (take (inc turns) (iterate #(update % :direction mapping) state)))))

(defn step-ship [state [action amount]]
  (case action
    "N" (update state :y + amount)
    "S" (update state :y - amount)
    "E" (update state :x + amount)
    "W" (update state :x - amount)
    "L" (turn state left-turns amount)
    "R" (turn state right-turns amount)
    "F" (step-ship state [(:direction state) amount])))

(defn rotate-right [[[n e] 
                     [w s]]]  
  [[w n]
   [s e]])

(defn rotate-left [[[n e] 
                    [w s]]]
  [[e s]
   [n w]])

(defn north [[[n _] _]] n)
(defn south [[_ [_ s]]] s)
(defn east [[[_ e] _]] e)
(defn west [[_ [w _]]] w)

(def unit-movements {"N" [[1 0] [0 0]]
                     "E" [[0 1] [0 0]]
                     "S" [[0 0] [0 1]]
                     "W" [[0 0] [1 0]]})

(defn waypoint-movement [[action amount]]
  (let [unit-movement (get unit-movements action)]
    (map (partial map (partial * amount)) unit-movement)))

(defn apply-waypoint-movement [waypoint movement]
  (map (partial map +) waypoint movement))

(defn rotate-waypoint [waypoint rotation degrees]
  (let [rotations (mod (/ degrees 90) 4)]
    (last (take (inc rotations) (iterate rotation waypoint)))))

(defn step-both [state [action amount]]
  (case action
    "N" (update state :waypoint apply-waypoint-movement (waypoint-movement [action amount]))
    "S" (update state :waypoint apply-waypoint-movement (waypoint-movement [action amount]))
    "E" (update state :waypoint apply-waypoint-movement (waypoint-movement [action amount]))
    "W" (update state :waypoint apply-waypoint-movement (waypoint-movement [action amount]))
    "L" (update state :waypoint rotate-waypoint rotate-left amount)
    "R" (update state :waypoint rotate-waypoint rotate-right amount)
    "F" (let [waypoint (:waypoint state)] 
          (-> state
              (step-ship ["N" (* amount (north waypoint))])
              (step-ship ["E" (* amount (east waypoint))])
              (step-ship ["S" (* amount (south waypoint))])
              (step-ship ["W" (* amount (west waypoint))])))))


(defn first-answer []
  (let [{x :x y :y} (reduce step-ship {:x 0 :y 0 :direction "E"} (read-directions)))]
    (+ (Math/abs x) (Math/abs y))))

(defn second-answer []
  (let [{x :x y :y} (reduce step-both {:x 0 :y 0 :waypoint [[1 10] [0 0]]} (read-directions))]
    (+ (Math/abs x) (Math/abs y))))
