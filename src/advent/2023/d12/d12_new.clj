(ns advent.2023.d12.d12-new
  (:require [clojure.string :as str]))

(defn- parse-line [line]
  (let [[springs numbers] (str/split line #"  *")
        numbers (read-string (str "[" (str/replace numbers #"," " ") "]"))]
    [(str/split springs #"") numbers]))

(defn parse [input]
  (vec (for [line (str/split-lines input)]
         (parse-line line))))

(defn- score
  [[dots blocks]]
  (let [m-f (memoize
             (fn
            ;;    i ranges over the dots
            ;;    bi ranges over the blocks"
            ;;    block-len is the length of the block of #s
               [f i bi block-len]
               (if (= i (count dots)) ;; end of dots
                 (cond
                   (and (= bi (count blocks)) ;; end of blocks
                        (= 0 block-len))
                   1

                   (and (= bi (dec (count blocks))) ;; last block
                        (= block-len (nth blocks bi))) ;; equal to block-len
                   1

                   :else 0)

                 (apply +
                        (for [c ["." "#"]]
                        ;; each character in the final string
                        ;; will be either a . or #
                          (if (or (= c (nth dots i)) ;; if the char is c (1) . (2) #
                                  (= "?" (nth dots i))) ;; or "?" which can stand for both.
                            (cond
                              (and (= c ".") (= 0 block-len)) ;; no block yet
                              (f f (inc i) bi 0)

                              (and (= c ".") (> block-len 0) ;; a block ended
                                   (< bi (count blocks)) ;; within the blocks array
                                   (= block-len (nth blocks bi))) ;; equal to the length
                              (f f (inc i) (inc bi) 0)

                              (= c "#") ;; in a block
                              (f f (inc i) bi (inc block-len))

                              :else 0 ; invalid
                              )
                            0))))))
        score-m (partial m-f m-f)]
    (score-m 0 0 0)))

(defn- parse-line-2 [l]
  (let [[springs numbers] (parse-line l)]
    [(vec (concat springs ["?"] springs ["?"] springs ["?"] springs ["?"] springs))
     (vec (concat numbers numbers numbers numbers numbers))]))

(defn parse-2 [input]
  (vec (for [line (str/split-lines input)]
         (parse-line-2 line))))


(defn -main [& _]
  (let [input (slurp "src/advent/2023/d12/input.txt")]
    (time (println "Day 1, Part 1:" (apply + (map score (parse input)))))
    (time (println "Day 1, Part 2:" (apply + (map score (parse-2 input)))))))