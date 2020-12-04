(ns four
  (:require [clojure.string :as str]))

(def REQUIRED-KEYS ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])
(def ALLOWED-EYE-COLORS #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn unparsed-passports []
  (filter not-empty 
          (str/split (slurp "resources/four.txt") #"\n\n")))

(defn parse-passport [unparsed-passport]
  (->> unparsed-passport
       (re-seq #"([a-z]{3}):([^\s]+)")
       (map rest)
       (map vec)
       (into {})))

(defmulti validate-height
  (fn [_ units] units))

(defmethod validate-height "cm" [height _]
  (<= 150 height 193))

(defmethod validate-height "in" [height _]
  (<= 59 height 76))

(defmethod validate-height :default [& _]
  false)

(defmulti validate-passport-key
  (fn [_ key] key))

(defmethod validate-passport-key "byr" [passport _]
  (let [value (get passport "byr")
        birth-year (Integer/parseInt value)]
    (<= 1920 birth-year 2002)))

(defmethod validate-passport-key "iyr" [passport _]
  (let [value (get passport "iyr")
        issue-year (Integer/parseInt value)]
    (<= 2010 issue-year 2020)))

(defmethod validate-passport-key "eyr" [passport _]
  (let [value (get passport "eyr")
        expiry-year (Integer/parseInt value)]
    (<= 2020 expiry-year 2030)))

(defmethod validate-passport-key "hgt" [passport _]
  (let [value (get passport "hgt")
        [height-value units] (vec (rest (first (re-seq #"(\d+)([^\d]*)" value))))]
    (validate-height (Integer/parseInt height-value) units)))

(defmethod validate-passport-key "hcl" [passport _]
  (let [value (get passport "hcl")]
    (re-matches #"#[a-f0-9]{6}" value)))

(defmethod validate-passport-key "ecl" [passport _]
  (let [value (get passport "ecl")]
    (ALLOWED-EYE-COLORS value)))

(defmethod validate-passport-key "pid" [passport _]
  (let [value (get passport "pid")]
    (re-matches #"[0-9]{9}" value)))

(defmethod validate-passport-key :default [& _]
  true)

(defn has-required-keys? [passport]
  (every? (partial get passport) REQUIRED-KEYS))

(defn keys-meet-criteria? [passport]
  (every? (partial validate-passport-key passport) REQUIRED-KEYS))

(defn valid-passport? [passport]
  (and (has-required-keys? passport)
       (keys-meet-criteria? passport)))

(defn first-solution []
  (->> (unparsed-passports)
       (map parse-passport)
       (filter has-required-keys?)
       (count)))

(defn second-solution []
  (->> (unparsed-passports)
       (map parse-passport)
       (filter valid-passport?)
       (count)))
