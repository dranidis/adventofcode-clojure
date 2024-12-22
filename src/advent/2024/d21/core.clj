(ns advent.2024.d21.core
  (:require
   [advent.2023.d17.priority-queue :refer [add-with-priority! extract-min!
                                           make-priority-queue!]]
   [advent.util :refer [coords-of-symbol str->2D str->num]]
   [clojure.string :as str]))

(def example? false)
(def example "029A
980A
179A
456A
379A")

(def input (if example? example (slurp "src/advent/2024/d21/input.txt")))
(def codes (str/split-lines input))

(defn vector-product
  [vectors]
  (if (empty? vectors)
    [[]]
    (vec (for [x (first vectors)
               y (vector-product (rest vectors))]
           (vec (concat [x] y))))))

(def digit-keypad
  (str->2D
   "789
    456
    123
    #0A"))

(def dir-keypad
  (str->2D
   "#^A
    <v>"))

(defn keypad-keys
  [keypad]
  (remove #(= % "#") (flatten keypad)))

(defn keypad-pos-of-char
  [keypad]
  (fn [char]
    (first (coords-of-symbol keypad char))))

(defn keypad-next
  [keypad]
  (fn [[r c]]
    (for [[cmd dr dc] [["^" -1 0] ["v" 1 0] ["<" 0 -1] [">" 0 1]]
          :let [nr (+ r dr)
                nc (+ c dc)
                cell (get-in keypad [nr nc])]
          :when (and (some? cell)
                     (not= cell "#"))]
      [[nr nc] cmd])))

(defn find-paths
  [neighbors [x0 y0] [x1 y1]]
  (let [dist (+ (abs (- x1 x0)) (abs (- y1 y0)))]
    (loop [Q (add-with-priority! (make-priority-queue! (fn [[d _] [d2 _]] (< d d2)))
                                 [0 [[x0 y0] []]])
           valid-paths []]
      (if-let [[d [[x y] path]] (extract-min! Q)]
        (cond
          (= [x y] [x1 y1])
          (recur Q (conj valid-paths (conj path "A")))

          (<= d dist) ;; is this necessary?
          (let [ns (neighbors [x y])
                Q (apply add-with-priority! Q
                         (map (fn [[[nr nc] c]]
                                [(inc d) [[nr nc] (conj path c)]])
                              ns))]
            (recur Q valid-paths))

          :else (recur Q valid-paths))
        (map (partial apply str) valid-paths)))))

(defn filter-multiple-turns
  [strings]
  (let [pattern #"v+[<>]+v+|\^+[<>]+\^+|>+[\^v]+>+|<+[v\^]+<+"]
    (filter #(not (re-find pattern %)) strings)))

(defn paths-from-to
  [keypad]
  (let [digits (keypad-keys keypad)]
    (into {}
          (for [digit-1 digits
                digit-2 digits
                :let [pos-1 ((keypad-pos-of-char keypad) digit-1)
                      pos-2 ((keypad-pos-of-char keypad) digit-2)]]
            [[pos-1 pos-2] (filter-multiple-turns
                            (find-paths (keypad-next keypad) pos-1 pos-2))]))))

(def paths-from-to-digit-keypad (paths-from-to digit-keypad))
(def paths-from-to-dir-keypad (paths-from-to dir-keypad))

(defn gen-sequences
  [keypad paths-from-to]
  (fn [code]
    (map (partial apply str)
         (vector-product
          (mapv (fn [[fc tc]]
                  (paths-from-to [((keypad-pos-of-char keypad) (str fc))
                                  ((keypad-pos-of-char keypad) (str tc))]))
                (partition 2 1 (concat "A" code)))))))

(def gen-sequences-digit-keypad (gen-sequences digit-keypad paths-from-to-digit-keypad))

(def gen-sequences-dir-keypad (gen-sequences dir-keypad paths-from-to-dir-keypad))

(defn split-A
  [dirs]
  (re-seq #"[^A]*A" dirs))

(defn- find-seq
  [code]
  (ffirst (sort-by second
                   (for [d (gen-sequences-dir-keypad code)
                         e (gen-sequences-dir-keypad d)
                         f (gen-sequences-dir-keypad e)]
                     [d [(count f) (count e) (count d)]]))))

(defn- transform-freqmap
  [sequence-fn freq-map]
  (->> freq-map
       (mapcat (fn [[sub-s cnt]] (->> (sequence-fn sub-s)
                                      (map (fn [[v0 c0]] [v0 (* cnt c0)])))))
       (reduce (fn [acc [v c]] (update acc v (fnil + 0) c)) {})))

(defn freq-transitions
  [n]
  (let [m-freq (memoize (fn [code]
                          (frequencies
                           (split-A
                            (find-seq code)))))]
    (fn [freq-map]
      (first (drop n (iterate (fn [freq-map]
                                (transform-freqmap m-freq freq-map))
                              freq-map))))))

(defn- total-length-of-sequence
  [freq-map]
  (apply + (map (fn [[sub-sequence cnt]] (* (count sub-sequence) cnt))
                freq-map)))

(defn calculate-length
  [n]
  (fn [code]
    (let [freqs-for-num (map frequencies
                             (map split-A
                                  (gen-sequences-digit-keypad code)))]
      (apply min (map total-length-of-sequence
                      (map (freq-transitions n)
                           freqs-for-num))))))

(defn answer
  [n]
  (apply +
         (map (fn [[n l]] (* n l))
              (map (juxt str->num (calculate-length n))
                   codes))))

(defn- -main [& _]
  (println "Day 21, Part 1:" (answer 2))
  (println "Day 21, Part 2:" (answer 25)))