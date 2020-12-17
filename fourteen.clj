(ns fourteen
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clojure.math.combinatorics :as combo]))

(defn bits->number [mask]
  (let [powers (reverse (range 0 36))]
    (reduce (fn [total [bit power]]
              (if (= 0 bit)
                total
                (+ total (math/expt 2 power))))
            0
            (map list mask powers))))

(defn number->bits [n]
  (let [powers (reverse (range 0 36))]
    (reduce (fn [mask power]
              (let [total-so-far (mask->number mask)
                    remaining (- n total-so-far)]
                (if (> (math/expt 2 power) remaining)
                  (conj mask 0)
                  (conj mask 1))))
            []
            powers)))

(defn parse-mask [s]
  (mapv #(if (= \X %) \X (Integer/parseInt (str %))) s))

(defn parse-line [line]
  (if-let [[[_ mask-string]] (re-seq #"mask = (.+)" line)]
    {:mask (parse-mask mask-string)}
    (let [[[_ address-string value-string]] (re-seq #"mem\[(\d+)\] = (\d+)" line)]
      {:address (Integer/parseInt address-string)
       :value (Integer/parseInt value-string)})))

(defn apply-mask [mask bits]
  (mapv (fn [mask-value bit]
          (if (= \X mask-value)
            bit
            mask-value))
        mask
        bits))

(defn floating-addresses [mask unmasked-address]
  (let [indexed-floaters (remove nil? (map-indexed (fn [index mask-value] (when (= \X mask-value) [index mask-value])) mask))
        value-variations (apply combo/cartesian-product (take (count indexed-floaters) (repeat [0 1])))
        replacements (map (fn [variation] (map (fn [replacement [index _]] [index replacement]) variation indexed-floaters)) value-variations)]
    (->> replacements
         (map (fn [replacement]
                (reduce #(apply assoc %1 %2)
                        unmasked-address
                        replacement)))
         (map (fn [partially-masked-address]
                (map (fn [address-bit mask-bit]
                       (if (= mask-bit 1)
                         1
                         address-bit))
                     partially-masked-address
                     mask)))
         (map bits->number))))

(defn interpret [lines]
  (reduce
   (fn [state line]
     (if (:mask line)
       (merge state line)
       (let [{address :address
              value :value} line
             mask (:mask state)]
         (assoc-in state [:addresses address] (bits->number (apply-mask mask (number->bits value)))))))
   {:addresses {}}
   (map parse-line lines)))

(defn interpret-v2 [lines]
  (reduce
   (fn [state line]
     (if (:mask line)
       (merge state line)
       (let [{address :address
              value :value} line
             mask (:mask state)
             floating-addresses (floating-addresses mask (number->bits address))]
         (reduce (fn [state* floating-address]
                   (assoc-in state* [:addresses floating-address] value))
                 state
                 floating-addresses))))
   {:addresses {}}
   (map parse-line lines)))

(defn first-answer []
  (->> (interpret (str/split-lines (slurp "resources/fourteen.txt")))
       :addresses
       vals
       (reduce +)))

(defn second-answer []
  (->> (interpret-v2 (str/split-lines (slurp "resources/fourteen.txt")))
       :addresses
       vals
       (reduce +)))
