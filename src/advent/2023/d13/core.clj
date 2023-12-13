(ns advent.2023.d13.core
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.set :as set]))

(def input (slurp "src/advent/2023/d13/input.txt"))

(def input "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn transpose [m]
  (apply mapv vector m))

(defn- parse-pattern [pattern]
  (for [line (str/split-lines pattern)]
    line))

(defn parse [input]
  (vec (for [pattern (str/split input #"\n\n")]
         (parse-pattern pattern))))

;; (defn mirrored-rec? [pattern l r]
;; ;;   (println pattern l r)
;;   (if (>= l r)
;;     false
;;     (let [fi (nth pattern l)
;;           la (nth pattern r)
;;         ;;   _ (println "comparing" fi la)
;;           ]
;;       (and (= fi la) (or (= (inc l) r) (mirrored-rec? pattern (inc l) (dec r)))))))

;; (comment
;;   (defn mirrored? [pattern]
;;     (letfn []
;;       (mirrored-rec? pattern 0 (dec (count pattern)))))

;;   (is (mirrored? "abba"))
;;   (is (not (mirrored? "dabba")))
;;   ;
;;   )

;; (defn reflection-left [pattern left-index right-index]
;;   (if (mirrored-rec? pattern left-index right-index)
;;     (inc (quot (+ right-index left-index) 2))
;;     (if (>= left-index right-index)
;;       nil
;;       (reflection-left pattern (inc left-index) right-index))))

;; (defn reflection-right [pattern left-index right-index]
;;   (if (mirrored-rec? pattern left-index right-index)
;;     (inc (quot (+ right-index left-index) 2))
;;     (if (>= left-index right-index)
;;       nil
;;       (reflection-right pattern left-index (dec right-index)))))

;; (defn reflection-p [pattern]
;;   (let [ref-left (reflection-left pattern 0 (dec (count pattern)))
;;         ref-right (reflection-right pattern 0 (dec (count pattern)))]
;;     (if (nil? ref-left) ref-right ref-left)))


;; (is (= 2 (reflection-p "abba")))
;; (is (= nil (reflection-p "abcb")))
;; (is (= nil (reflection-p "aceecdb"))) ; rows of first pattern, should fail

;; (is (= 5 (reflection-p (transpose (nth (parse input) 0)))))
;; (is (= 4 (reflection-p (nth (parse input) 1))))

;; (is (nil? (reflection-p (nth (parse input) 0))))
;; (is (nil? (reflection-p (transpose (nth (parse input) 1)))))

;; (defn answer [input] (let  [a1 (apply + (remove nil? (map reflection-p (parse input))))
;;                             a2 (apply + (remove nil? (map (fn [p] (reflection-p (transpose p))) (parse input))))]
;;                        (+ (* a1 100) a2)))

;; (is (= 405 (answer input)))
;; (is (= 34772 (answer (slurp "src/advent/2023/d13/input.txt"))))



;; part 2

;; find fi and la that have only one difference
;; swap . #
;; don't swap again

(comment

  (def fi "#.##..##.")
  (def la "..##..##.")
  (dec (count fi))

  (count (set/intersection (set (filter (fn [[i c]]
                                          (= c \#)) (map-indexed vector fi)))
                           (set (filter (fn [[i c]]
                                          (= c \#)) (map-indexed vector la)))))

  (count (set/intersection (set
                            (map-indexed vector fi))
                           (set
                            (map-indexed vector la))))
  ;
  )


(defn- smudged? [fi la]
  (= (dec (count fi))
     (count (set/intersection (set
                               (map-indexed vector fi))
                              (set
                               (map-indexed vector la))))))

(is (smudged? "#.##..##." "..##..##."))

(comment
  (def fi "#.##..##.")
  (filter (fn [[i c]]
            (= c \#)) (map-indexed vector fi))

  (def la "#.#.##.#.")
  (filter (fn [[i c]]
            (= c \#)) (map-indexed vector la))

  (count (set/intersection (set (filter (fn [[i c]]
                                          (= c \#)) (map-indexed vector fi)))
                           (set (filter (fn [[i c]]
                                          (= c \#)) (map-indexed vector la)))))
  ;
  )


;; (defn mirrored-rec? [pattern l r]
;; ;;   (println pattern l r)
;;   (if (>= l r)
;;     false
;;     (let [fi (nth pattern l)
;;           la (nth pattern r)
;;         ;;   _ (println "comparing" fi la)
;;           ]
;;       (and (= fi la) (or (= (inc l) r) (mirrored-rec? pattern (inc l) (dec r)))))))

(defn mirrored-rec-smudge? [pattern l r swapped?]
  (println "mirr?" pattern l r swapped?)
  (if (>= l r)
    false
    (let [fi (nth pattern l)
          la (nth pattern r)
        ;;   _ (println "comparing" fi la)
          was-smud? swapped?
          smud? (if (not swapped?)
                  (smudged? fi la)
                  swapped?)]
      (if was-smud?
        (and (= fi la) (or (= (inc l) r) (mirrored-rec-smudge? pattern (inc l) (dec r) true)))
        (if smud?
          (or (= (inc l) r) (mirrored-rec-smudge? pattern (inc l) (dec r) true))
          (and (= fi la)
               (or (= (inc l) r) (mirrored-rec-smudge? pattern (inc l) (dec r) smud?))))))))


(defn- reflection-left-smudge [pattern left-index right-index]
  (println "reflection-left-smudge" left-index right-index)
  (if (mirrored-rec-smudge? pattern left-index right-index false)
    (inc (quot (+ right-index left-index) 2))
    (if (>= left-index right-index)
      nil
      (reflection-left-smudge pattern (inc left-index) right-index))))

(defn- reflection-right-smudge [pattern left-index right-index]
  (println "reflection-right-smudge" left-index right-index)
  (if (mirrored-rec-smudge? pattern left-index right-index false)
    (inc (quot (+ right-index left-index) 2))
    (if (>= left-index right-index)
      nil
      (reflection-right-smudge pattern  left-index (dec right-index)))))

(defn- reflection-p-smudge [pattern]
  (let [ref-left (reflection-left-smudge pattern 0 (dec (count pattern)))
        ref-right (reflection-right-smudge pattern 0 (dec (count pattern)))]
    (if (nil? ref-left) ref-right ref-left)))


(is (= 3 (reflection-p-smudge (nth (parse input) 0)))) ; horizontal
(is (= 1 (reflection-p-smudge (nth (parse input) 1)))) ; horizontal

((def pattern (nth (parse input) 0))

 (reflection-right-smudge pattern 0 (dec (count pattern))))

(defn answer-2 [input] (let  [a1 (apply + (remove nil? (map reflection-p-smudge (parse input))))
                              a2 (apply + (remove nil? (map (fn [p] (reflection-p-smudge (transpose p))) (parse input))))]
                         (+ (* a1 100) a2)))

(answer-2 input)
(comment
  (apply + (remove nil? (map reflection-p-smudge (parse input))))

  (apply + (remove nil? (map (fn [p] (reflection-p-smudge (transpose p))) (parse input)))))