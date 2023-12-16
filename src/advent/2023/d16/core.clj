(ns advent.2023.d16.core
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(def input ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c line]
                (str c))))))

(defn flip-directions [dir-r dir-c ch]
  (case ch
    "\\" [dir-c dir-r]
    "/" [(* -1 dir-c) (* -1 dir-r)]
    :else "Unknown flip"))

(def visited-m (atom #{}))

(defn light-beam [pi rows cols start-r start-c energized start-dir-r start-dir-c]
  (loop [r start-r
         c start-c
         energized energized
         dir-r start-dir-r
         dir-c start-dir-c]
    (if (and (< -1  r rows) (< -1  c cols) (not (@visited-m [r c dir-r dir-c])))
      (let [ch (get-in pi [r  c])
            _ (swap! visited-m conj [r c dir-r dir-c])]
        (case ch
          "." (recur (+ r dir-r) (+ c dir-c) (conj energized [r c]) dir-r dir-c)

          ("\\" "/") (let [[dir-r dir-c] (flip-directions dir-r dir-c ch)]
                       (recur (+ r dir-r) (+ c dir-c)  (conj energized [r c]) dir-r dir-c))

          "|" (if (= dir-r 0)
                (let [to-dn (light-beam pi rows cols (inc r) c
                                        (conj energized [r c])
                                        1 0)
                      to-up (light-beam pi rows cols (dec r) c
                                        (conj energized [r c])
                                        -1 0)]
                  (set/union (conj energized [r c]) to-dn to-up))
                (recur (+ r dir-r) (+ c dir-c)  (conj energized [r c]) dir-r dir-c))

          "-" (if (= dir-c 0)
                (let [to-rt (light-beam pi rows cols r (inc c)
                                        (conj energized [r c])
                                        0 1)
                      to-lf (light-beam pi rows cols r (dec c)
                                        (conj energized [r c])
                                        0 -1)]
                  (set/union (conj energized [r c]) to-rt to-lf))
                (recur (+ r dir-r) (+ c dir-c)  (conj energized [r c]) dir-r dir-c))
          (prn "no math" ch)))
      energized)))


(defn all [input r c dir-r dir-c]
  (let [pi (str->2D input)
        rows (count pi)
        cols (count (first pi))]
    (reset! visited-m #{})
    ;; (prn rows cols visited-m)
    ;; (pp/pprint pi)

    (light-beam pi rows cols r c #{} dir-r dir-c)))

(is (= 51 (count (all input 0 3 1 0))))


(defn confs [input]
  (let [pi (str->2D input)
        rows (count pi)
        cols (count (first pi))
        all-rows (apply concat (for [r (range rows)]
                                 [[r 0 0 1] [r (dec cols) 0 -1]]))
        all-cols (apply concat (for [c (range cols)]
                                 [[0 c 1 0] [(dec rows) c -1 0]]))]
    (apply conj all-rows all-cols)))

(comment

  (apply max (map (fn [[r c dir-r dir-c]]
                    (count (all input r c dir-r dir-c))) (confs input)))
  ;
  )

(defn answer-1 [input]
  (count (all input 0 0 0 1)))


(defn answer-2 [input]
  (apply max (map (fn [[r c dir-r dir-c]]
                    (count (all input r c dir-r dir-c))) (confs input))))

(defn -main [& _]
  (let [input (slurp "src/advent/2023/d16/input.txt")]
    (time (println "Day 16 Part 1" (answer-1 input)))
    (time (println "Day 16 Part 2" (answer-2 input)))))

;; Day 16 Part 1 7562
;; "Elapsed time: 289.800896 msecs"
;; Day 16 Part 2 7793
;; "Elapsed time: 58860.11444 msecs"
