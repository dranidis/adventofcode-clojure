(ns advent.2020.d24.core
  (:require
   [advent.util :refer [draw-grid grid-2d set-grid]]
   [clojure.string :as str]))

(def example? false)

(def example "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(def input (if example? example (slurp "src/advent/2020/d24/input.txt")))

(defn ptile [line]
  (vec (re-seq #"se|sw|nw|ne|e|w" line)))

(def dir-tiles
  (->> input
       str/split-lines
       (mapv ptile)))

(defn tile-after [dirs]
  (loop [dirs dirs
         r 0
         c 0]
    (if (empty? dirs)
      [r c]
      (let [f (first dirs)
            [r c] (case f
                    "ne" [(dec r) c]
                    "e" [r (inc c)]
                    "se" [(inc r) (inc c)]
                    "sw" [(inc r) c]
                    "w" [r (dec c)]
                    "nw" [(dec r) (dec c)])]
        (recur (rest dirs) r c)))))

(def all-tiles (->> dir-tiles
                    (mapv tile-after)))

(def black (->> all-tiles
                (reduce (fn [d co]
                          ;; (prn d)
                          (if (d co)
                            (disj d co)
                            (conj d co)))
                        #{})))
(->> black
     count
     (println "ANS 1: "))


(comment
  ;; only for debugging the hex
  (defn draw-gr [b s tr]
    (-> (grid-2d s s ".")
        (set-grid
         (->> b
              (map (fn [t] (map + t [tr tr]))))
         "B")
      ;; (draw-hex)
        draw-grid))

  (draw-gr black 10 5)
;                0 1 2 3 4 5 6 7 8 9 
;  0             . . . . . . . . . . 
;  1            . . . . . . . . . . 
;  2           . . . . . B . . . . 
;  3          . . . . . . . . . . 
;  4         . . . B B . . . . . 
;  5        . . . B . B . B . . 
;  6       . . . . B . . . . . 
;  7      . . . . B . . B . . 
;  8     . . . . . B . . . . 
;  9    . . . . . . . . . .   ;
  )

(def drcs [[0 -1] [0 1]
           [1 1] [-1 -1]
           [-1 0] [1 0]])

(->>
 (loop [black black
        cnt 0]
  ;;  (draw-gr black 120 60)
   (if (= 100 cnt)
     black
     (let [to-be-removed
           (for [b black
                 :let [cnt-n-black (->> drcs
                                        (mapv #(map + b %))
                                        (filter (fn [n] (black n)))
                                        count)]
                 :when (or (zero? cnt-n-black)
                           (> cnt-n-black 2))]
             b)

           to-be-added
           (set (for [cand (mapcat
                            (fn [b]
                              (->> drcs
                                   (mapv #(mapv + b %)))) black)
                      :when (not (black cand))
                      :let [cnt-n-black (->> drcs
                                             (mapv #(map + cand %))
                                             (filter (fn [n] (black n)))
                                             count)]
                      :when (= 2 cnt-n-black)]
                  cand))

           black (-> black
                     (#(apply disj % to-be-removed))
                     (#(apply conj % to-be-added)))]
       (recur black (inc cnt)))))
 count
 (println "ANS 2: "))
