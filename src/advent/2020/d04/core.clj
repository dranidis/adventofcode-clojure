(ns advent.2020.d04.core
  (:require
   [clojure.string :as str]))

(def passports (str/split (slurp "src/advent/2020/d04/input.txt") #"\n\n"))

(defn valid-fields?
  [p]
  (every? true?
          (map #(str/includes? p (str % ":"))
               ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])))

(->> passports
     (filter valid-fields?)
     count
     println)

(defn- valid-values?
  [passport]
  (let [[[_ byr]] (re-seq #"byr:(\d+)" passport)
        [[_ iyr]] (re-seq #"iyr:(\d+)" passport)
        [[_ eyr]] (re-seq #"eyr:(\d+)" passport)
        [[_ hgt unit]] (re-seq #"hgt:(\d+)(cm|in)" passport)
        [[_ hcl]] (re-seq #"hcl:#([0-9a-f]*)" passport)
        [[_ ecl]] (re-seq #"ecl:(amb|blu|brn|gry|grn|hzl|oth)" passport)
        [[_ pid]] (re-seq #"pid:([0-9]*)" passport)]
    (and (<= 1920 (parse-long byr) 2022)
         (<= 2010 (parse-long iyr) 2020)
         (<= 2020 (parse-long eyr) 2030)
         (or (and (= unit "cm") (<= 150 (parse-long hgt) 193))
             (and (= unit "in") (<= 59 (parse-long hgt) 76)))
         (= 6 (count hcl))
         (some? ecl)
         (= 9 (count pid)))))

(->> passports
     (filter valid-fields?)
     (filter valid-values?)
     count
     println)

