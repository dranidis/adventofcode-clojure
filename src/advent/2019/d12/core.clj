(ns advent.2019.d12.core
  (:require
   [advent.util :refer [lcm]]
   [clojure.string :as str]))

(def moons (->> (str/split-lines (slurp "src/advent/2019/d12/input.txt"))
                (mapv (fn [l] (->> l
                                   (re-seq #"-?\d+")
                                   (mapv parse-long))))))

(def velocities (vec (repeat (count moons) [0 0 0])))

(defn pull-d [moon1 moon2]
  (mapv (fn [x1 x2] (cond (> x1 x2) 1 (< x1 x2) -1 :else 0)) moon1 moon2))

(defn velocities-fn [moons]
  (->> moons
       (mapv #(apply mapv +
                     (mapv (fn [m] (pull-d m %)) moons)))))

(defn update-moons-velocities [times]
  (loop [moons moons
         velocities velocities
         t 0]
    (if (= times  t)
      [moons velocities]
      (let [upd-velocities (velocities-fn moons)
            new-velocities (mapv #(mapv + %1 %2) upd-velocities velocities)
            new-moons (mapv #(mapv + %1 %2) moons new-velocities)]
        (recur new-moons new-velocities (inc t))))))

(defn abs-sum [m] (apply + (mapv abs m)))

(defn energy [[moons velocities]]
  (->> [moons velocities]
       (mapv (partial mapv abs-sum))
       (apply mapv *)
       (apply +)))

(println "ANS 1: " (energy (update-moons-velocities 1000)))

(defn pos-vels [moons velocities]
  (mapv (fn [i] (mapv #(nth % i) (vec (concat moons velocities)))) [0 1 2]))

(defn update-D [tx Xs xs t]
  (let [tx (if (and (nil? tx) (contains? Xs xs)) (- t (Xs xs)) tx)
        Xs (if (nil? tx) (assoc Xs xs t) Xs)]
    [tx Xs]))

(defn repeat-moons-velocities []
  (let [[xs ys zs] (pos-vels moons velocities)]
    (loop [moons moons
           velocities velocities
           Xs {xs 0}
           Ys {ys 0}
           Zs {zs 0}
           tx nil
           ty nil
           tz nil
           t 0]
      (if (and (some? tx) (some? ty) (some? tz))
        [tx ty tz]
        (let [upd-velocities (velocities-fn moons)
              new-velocities (mapv #(mapv + %1 %2) upd-velocities velocities)
              new-moons (mapv #(mapv + %1 %2) moons new-velocities)
              [xs ys zs] (pos-vels new-moons new-velocities)
              [tx Xs] (update-D tx Xs xs t)
              [ty Ys] (update-D ty Ys ys t)
              [tz Zs] (update-D tz Zs zs t)]
          (recur new-moons new-velocities Xs Ys Zs tx ty tz (inc t)))))))

(println "ANS 2: " (reduce lcm (map inc (repeat-moons-velocities))))

