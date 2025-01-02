(ns advent.2020.d20.core
  (:require
   [advent.util :refer [coords-of-symbol in-vector? str->2D str->nums
                        transpose]]
   [clojure.string :as str]
   [clojure.math :as math]))

(def sections (str/split (slurp "src/advent/2020/d20/input.txt") #"\n\n"))

(defn- parse-tile [arg1]
  (let [ls (str/split-lines arg1)]
    [(->> (first ls) (str->nums) first)
     (->> (vec (for [l (rest ls)]
                 (vec (for [c l]
                        (str c))))))]))

(def tiles (->> sections
                (map parse-tile)
                (into {})))

(def tile-size (count (first (second (first tiles)))))
(def grid-size (long (math/sqrt (count tiles))))


(defn borders [tile]
  (let [t-tile (transpose tile)]
    [(first tile)
     (vec (reverse (first tile)))
     (first t-tile)
     (vec (reverse (first t-tile)))
     (last tile)
     (vec (reverse (last tile)))
     (last t-tile)
     (vec (reverse (last t-tile)))]))

(def tile-borders (->> sections
                       (map parse-tile)
                       (map (fn [[id t]] [id (borders t)]))
                       (into {})))

(def matching (for [[id1 bs1] tile-borders
                    [id2 bs2] tile-borders
                    :when (> id1 id2)
                    b1 bs1
                    b2 bs2
                    :when (= b1 b2)]
                [id1 id2]))

(def match-dict (into {}
                      (for [[id bs] tile-borders
                            :let [m (->> matching
                                         (filter (fn [[id1 id2]] (#{id1 id2} id)))
                                         (map (fn [[id1 id2]] (if (= id1 id) id2 id1)))
                                         set)]]
                        [id m])))

(println "Size" tile-size)
(println "Grid size" grid-size)
(println "There are" (count tile-borders) "tiles")
(println "Each tile fits with "
         (->> match-dict
              vals
              (map count)
              distinct)
         "other tiles.")

(println "Frequencies of matchings: "
         (->> match-dict
              vals
              (map count)
              frequencies))

(defn matching-num-of-tiles [num]
  (->> match-dict
       (filter (fn [[_ v]] (= (count v) num)))
       (map first)))

(def corners (matching-num-of-tiles 2))

(println "ANS 1" (->> corners
                      (apply *)))

;; PART 2
(defn options [tile]
  (let [rev-rows (fn [t]
                   (mapv (fn [r] (reverse r)) t))]
    (->> tile
    ;;  second
         ((fn [t] (mapv vec [t
                             (reverse t)
                             (transpose t)
                             (reverse (transpose t))
                             (transpose (reverse t))
                             (rev-rows t)
                             (reverse (rev-rows t))
                             (transpose (rev-rows t))
                             (reverse (transpose (rev-rows t)))
                             (transpose (reverse (rev-rows t)))]))))))

(defn last-column [tile]
  (->> tile (mapv #(nth % (dec tile-size)))))

(defn first-column [tile]
  (->> tile (mapv #(nth % 0))))

(defn start-orientation [start-tile]
  (let [m (match-dict start-tile)]
    (assert (= 2 (count m)))
    (for [s (options (tiles start-tile))
          :let [next-tile (first m)
                down-tile (second m)]
          n (options (tiles next-tile))
          d (options (tiles down-tile))
          :when (and (= (last-column s) (first-column n))
                     (= (last s) (first d)))]
      s)))

(defn- possible-tile-orientations [image tile r c]
  (if (= [r c] [0 0])
    (start-orientation tile) ;; I have to orient the first tile
    (if (> c 0)
      (if (= r 0)
        (let [lc (last-column (get-in image [r (dec c)]))]
          (for [o (options (tiles tile))
                :when (= (first-column o) lc)]
            o))

        (let [lc (last-column
                  (get-in image [r (dec c)]))
              lr (last (get-in image [(dec r) c]))]

          (for [o (options (tiles tile))
                :when (= (first-column o) lc)
                :when (= (first o) lr)]
            o)))

      (let [img (get-in image [(dec r) c])
            last-row (last img)]
        (for [o (options (tiles tile))
              :when (= (first o) last-row)]
          o)))))

(defn make-image []
  (let [edges (matching-num-of-tiles 3)
        middle (matching-num-of-tiles 4)]
    (loop [r 0
           c 0
           puzzle (vec (repeat grid-size []))
           image (vec (repeat grid-size []))
           visited #{}]
      (if (>= r grid-size)
        image
        (if (>= c grid-size)
          (recur (inc r) 0 puzzle image visited)
          (let [maxs (dec grid-size)
                cand-tiles (->> (if (#{[0 0] [0 maxs] [maxs 0] [maxs maxs]} [r c])
                                  corners
                                  (if (or (#{0 maxs} r) (#{0 maxs} c))
                                    edges
                                    middle))
                                (remove (fn [t] (visited t))))
                with-tiles (->> (map
                                 (fn [[dr dc]]
                                   (get-in puzzle [(+ r dr)
                                                   (+ c dc)]))
                                 [[0 -1] [-1 0]])
                                (remove nil?))
                matching (set (for [cand cand-tiles
                                    :when (every? (fn [t]
                                                    ((match-dict t) cand))
                                                  with-tiles)]
                                cand))
                _ (assert (or (#{[0 0] [0 1]} [r c]) (= 1 (count matching))))
                tile (first matching)
                puzzle (assoc-in puzzle [r c] tile)
                tile-orientations (possible-tile-orientations image tile r c)
                _ (assert (seq tile-orientations))
                tile-orientation (first tile-orientations)
                image (assoc-in image [r c] tile-orientation)
                visited (conj visited tile)]
            (recur r (inc c) puzzle image visited)))))))

(defn remove-borders [img]
  (reduce
   (fn [acc r]
     (conj acc
           (vec
            (reduce (fn [acc2 c]
                      (conj acc2
                            (vec
                             (->> c
                                  (drop 1)
                                  (take (- tile-size 2))
                                  (mapv
                                   (fn [rr] (->> rr (drop 1) (take (- tile-size 2)))))))))
                    []
                    r))))
   []
   img))

(defn connect-tiles [img]
  (reduce
   (fn [acc r]
     (loop [acc acc cnt 0]
       (if (= cnt 8) acc
           (recur (conj acc
                        (vec (mapcat (fn [g] (nth g cnt)) r)))
                  (inc cnt)))))
   []
   img))

(def sea-monster
  (-> "                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "
      (str/replace #" " ".")
      str->2D
      (coords-of-symbol "#")))

(defn coords-of-monsters
  [img]
  (->> (for [img (options img)
             :let [co-of-m
                   (for [r (range (count img))
                         c (range (count (first img)))
                         :when (every? (fn [d]
                                         (= "#" (get-in img (map + [r c] d))))
                                       sea-monster)]
                     [r c])]
             :when (seq co-of-m)]
         [img (vec co-of-m)])
       first))

(def img-and-monster-coords
  (-> (make-image) remove-borders connect-tiles coords-of-monsters))

(let  [[img m-co] img-and-monster-coords]
  (->> (coords-of-symbol img "#")
       (remove #(in-vector?
                 (vec (mapcat
                       (fn [co]
                         (mapv (fn [s]
                                 (mapv + s co)) sea-monster))
                       m-co))
                 %))
       count
       (println "ANS 2")))

