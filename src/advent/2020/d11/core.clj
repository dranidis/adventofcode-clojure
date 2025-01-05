(ns advent.2020.d11.core
  (:require
   [advent.util :refer [coords-of-symbol str->2D]]
   [clojure.set :as set]))

(def grid (str->2D (slurp "src/advent/2020/d11/input.txt")))
(def rows (count grid))
(def cols (count (first grid)))

(def empty-seats (set (coords-of-symbol grid "L")))

(defn neigh-occupied [_ occupied seat]
  (let [[r c] seat]
    (for [[dr dc] [[0 1] [0 -1] [1 0] [-1 0]
                   [-1 -1] [-1 1] [1 -1] [1 1]]
          :let [nr (+ r dr)
                nc (+ c dc)]
          :when (occupied [nr nc])]
      [nr nc])))

(defn- apply-rules [neigh-occupied threshold [empty-seats occupied]]
  (let [next-occupied (set
                       (for [seat empty-seats
                             :when (empty? (neigh-occupied empty-seats occupied seat))]
                         seat))
        next-empty (set
                    (for [seat occupied
                          :when (>= (count (neigh-occupied empty-seats
                                                           occupied seat))
                                    threshold)]
                      seat))]
    [(set/difference (set/union empty-seats next-empty) next-occupied)
     (set/union (set/difference occupied next-empty) next-occupied)]))

;; PART 1 answer
(let [[_ occup] (loop [state [empty-seats #{}]
                       prev [nil nil]]
                  (if (= state prev)
                    state
                    (recur (apply-rules neigh-occupied 4 state) state)))]
  (println (count occup)))

;; PART 2
(defn- neigh-occupied-2 [empty-seats occupied seat]
  (let [[r c] seat]
    (filter some?
            (for [[dr dc] [[0 1] [0 -1] [1 0] [-1 0]
                           [-1 -1] [-1 1] [1 -1] [1 1]]]
              (let [seats-at-directions
                    (for [x (drop 1 (range))
                          :let [nr (+ r (* x dr))
                                nc (+ c (* x dc))]
                          :while (and (<= 0 nr (dec rows))
                                      (<= 0 nc (dec cols))
                                      (not (empty-seats [nr nc])))
                          :when (occupied [nr nc])]
                      [nr nc])]
                (if (empty? seats-at-directions)
                  nil
                  (first seats-at-directions)))))))

;; answer
(let [[_ occup] (loop [state [empty-seats #{}]
                       prev [nil nil]]
                  (if (= state prev)
                    state
                    (recur (apply-rules neigh-occupied-2 5 state) state)))]
  (println (count occup)))
