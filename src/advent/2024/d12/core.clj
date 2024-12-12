(ns advent.2024.d12.core
  (:require
   [advent.util :refer [flood-fill str->2D]]))

;; (def input
;;   "EEEEE
;; EXXXX
;; EEEEE
;; EXXXX
;; EEEEE")

(def input (slurp "src/advent/2024/d12/input.txt"))

(def grid (str->2D input))
(def rows (count grid))
(def cols (count (first grid)))

(defn next-fn
  [r c]
  (for [[dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [nr (+ r dr) nc (+ c dc)]
        :when (= (get-in grid [nr nc])
                 (get-in grid [r c]))]
    [nr nc]))

(def regions
  (loop [r 0
         c 0
         visited #{}
         flood-fills #{}]
    (if (= r rows)
      flood-fills
      (if (= c cols)
        (recur (inc r) 0 visited flood-fills)
        (if-not (visited [r c])
          (let [visited (conj visited [r c])
                floodffill (flood-fill next-fn r c)
                flood-fills (conj flood-fills floodffill)]
            (recur r (inc c) visited flood-fills))
          (recur r (inc c) visited flood-fills))))))

(defn area
  [region]
  (count region))

(defn perimeter-points [region]
  (for [[r c] region
        [dr dc] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [nr (+ r dr) nc (+ c dc)]
                      ;; :when (<= 0 nr (dec rows))
                      ;; :when (<= 0 nc (dec cols))
        :when (not (region [nr nc]))]
    [r c nr nc]))

(defn perimeter
  [region]
  (count (perimeter-points region)))

(defn price
  [region]
  (* (area region) (perimeter region)))

(def answer-1 (apply + (map price regions)))

(println answer-1)

;; PART 2

(defn in-same-side?
  "Should not be used with empty sets"
  [side r c nr nc]
  (let [[sr sc snr snc] (first side)
        horizontal? (= c nc)]
    (if horizontal?
      (and (= nr snr)
           (= r sr)
           (some (fn [[sr sc snr snc]]
                   (= 1 (abs (- nc snc))))
                 side))
      (and (= nc snc)
           (= c sc)
           (some (fn [[sr sc snr snc]]
                   (= 1 (abs (- nr snr))))
                 side)))))

(defn sides
  [region]
  (let [all-per-points (sort (perimeter-points region))]
    (count (loop [per-points all-per-points
                  sides #{}]
             (if (empty? per-points)
               sides
               (let [[r c nr nc] (first per-points)

                     [matching-sides non-matching-sides]
                     [(filter (fn [side] (in-same-side? side r c nr nc)) sides)
                      (remove (fn [side] (in-same-side? side r c nr nc)) sides)]]

                 (if (empty? matching-sides)
                    ;; add new set
                   (recur (rest per-points) (conj sides #{[r c nr nc]}))
                    ;; add to the first matching set
                   (let [matching (first matching-sides)
                         new-sides (conj non-matching-sides (conj matching [r c nr nc]))]
                     (recur (rest per-points) new-sides)))))))))

(defn price-2
  [region]
  (* (area region) (sides region)))

(def answer-2 (apply + (map price-2 regions)))
(println answer-2)

(defn- -main [& _]
  (println "Day 9, Part 1:" answer-1)
  (println "Day 9, Part 2:" answer-2))