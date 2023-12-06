(ns advent.2023.d6
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input "Time:      7  15   30
Distance:  9  40  200")

(def day-input "Time:        47     70     75     66
Distance:   282   1079   1147   1062")

(defn read-a-line [line]
  (mapv read-string (re-seq #"\d+" line)))

(defn parse [input read-a-line]
  (mapv read-a-line (str/split-lines input)))

(defn- hold [hold-time duration]
  (* (- duration hold-time)
     hold-time))

(defn num-ways-to-win [duration record-distance]
  (count (filter (fn [hold-time]
                   (> (hold hold-time duration)
                      record-distance))
                 (range duration))))

(defn answer1 [[durations distances]]
  (apply * (map num-ways-to-win durations distances)))

(is (= 288 (answer1 (parse input read-a-line))))

;; part 2

(defn read-a-line-2 [line]
  (read-string (apply str (mapv read-string (re-seq #"\d+" line)))))

(defn answer2 [input]
  (apply num-ways-to-win (parse input read-a-line-2)))

(is (= 71503 (answer2 input)))
