(ns advent.2021.d21.core
  (:require
   [advent.util :refer [parse-lines-with-numbers]]))

(def example? false)

(def input (if example? "Player 1 starting position: 4
Player 2 starting position: 8"
               (slurp "src/advent/2021/d21/input.txt")))

(def parsed (parse-lines-with-numbers input))

(def state (let [positions (mapv second parsed)]
             {:current-player :p1
              :p1 {:score 0 :position (first positions)}
              :p2 {:score 0 :position (second positions)}}))

(defn other-player [player]
  (if (= player :p1) :p2 :p1))

(defn change-state
  [state roll]
  (let [current-player (:current-player state)
        position (get-in state [current-player :position])
        new-position (inc (mod (+ (dec position) roll) 10))
        state (assoc-in state [current-player :position] new-position)
        state (update-in state [current-player :score] + new-position)
        state (assoc state :current-player (other-player current-player))]
    state))

(def answer-1
  (loop [die (map inc (map (fn [r] (mod r 100)) (range)))
         state state
         times-rolled 0]
    ;; (prn state)
    (let [current-player (:current-player state)]
      (if
       (>= (get-in state [(other-player current-player) :score]) 1000)
        (* (get-in state [current-player :score]) times-rolled)

        (let [roll (apply + (take 3 die))
              state (change-state state roll)]
          (recur (drop 3 die) state (+ 3 times-rolled)))))))

;; 
;; FUNCTION TO BE MEMOIZED
;; 
;; (defn dirac
;;   [state]
;;   (let [current-player (:current-player state)
;;         opponent (other-player current-player)]
;;     (if
;;      (>= (get-in state [opponent :score]) 11)
;;       (if (= :p1 opponent)
;;         [1 0]
;;         [0 1])

;;       (let [games (for [roll1 [1 2 3]
;;                         roll2 [1 2 3]
;;                         roll3 [1 2 3]
;;                         roll [(+ roll1 roll2 roll3)]]
;;                     (dirac (change-state state roll)))]
;;         (apply (partial map +) games)))))

(defn dirac
  [state]
  (let [m-f (memoize
             (fn
               [f state]
               (let [current-player (:current-player state)
                     opponent (other-player current-player)]
                 (if
                  (>= (get-in state [opponent :score]) 21)
                   (if (= :p1 opponent)
                     [1 0]
                     [0 1])

                   (let [games (for [roll1 [1 2 3]
                                     roll2 [1 2 3]
                                     roll3 [1 2 3]
                                     roll [(+ roll1 roll2 roll3)]]
                                 (f f (change-state state roll)))]
                     (apply (partial map +) games))))))
        dirac-m (partial m-f m-f)]
    (dirac-m state)))

(def answer-2 (apply max (dirac state)))

(defn- -main [& _]
  (println "2021, Day 21, Part 1:" answer-1)
  (println "2021, Day 21, Part 2:" answer-2))



