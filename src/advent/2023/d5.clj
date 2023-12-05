(ns advent.2023.d5
  (:require [advent.2023.d5-input :refer [day-5-input]]
            [clojure.string :as s]
            [clojure.test :refer [is]]))

;; food production problem.

(def input "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")


(defn- parse-mapping [map-string]
  (let [[[_ source destination ranges]]
        (re-seq #"(.*)-to-(.*) map:(.*)" map-string)
        r (mapv (fn [s] (read-string (str "[" s "]"))) (remove empty? (s/split ranges #"-")))]
    {:source source
     :dest destination
     :ranges r}))

(defn parse [input]
  (let  [lines (s/split-lines input)
         seeds (mapv read-string (re-seq #"\d+" (first lines)))
         maps-string (remove empty? (s/split (apply str (map (fn [s] (if (= s "") "\nn" (str s "-")))
                                                             (rest lines))) #"\nn"))
         maps (group-by :source (map parse-mapping maps-string))]
    [seeds maps]))

(defn ranges-map [rs id]
  (let [l (filter (fn [r] (<= (second r) id (+ (second r) (nth r 2)))) rs)]
    (if (empty? l)
      id
      (let [range (first l)
            source-start (second range)
            dest-start (first range)]
        (+ (- id source-start) dest-start)))))

(is (= 81 (ranges-map [[50 98 2] [52 50 48]] 79)))
(is (= 14 (ranges-map [[50 98 2] [52 50 48]] 14)))
(is (= 57 (ranges-map [[50 98 2] [52 50 48]] 55)))
(is (= 13 (ranges-map [[50 98 2] [52 50 48]] 13)))

(defn map-id->id [maps map-key an-id]
  (ranges-map (:ranges (first (get maps map-key))) an-id))

(defn location [maps seed-id]
  (->> seed-id
       (map-id->id maps "seed")
       (map-id->id maps "soil")
       (map-id->id maps "fertilizer")
       (map-id->id maps "water")
       (map-id->id maps "light")
       (map-id->id maps "temperature")
       (map-id->id maps "humidity")
       (map-id->id maps "location")))

(is (= 82 (location (second (parse input)) 79)))
(is (= 43 (location (second (parse input)) 14)))

(defn lowest-location [input]
  (let [info (parse input)
        seeds (first info)
        maps (second info)]
    (apply min (map (fn [seed]
                      (location maps seed)) seeds))))

;; answer 1
(comment
  (lowest-location day-5-input))

(is (= 313045984 (lowest-location day-5-input)))

;; 
;; PART 2
;; 

(defn- expand [seeds]
  (let [seed-pairs (partition 2 seeds)
        all (mapcat (fn [[start len]]
                      (range start (+ start len)))
                    seed-pairs)]
    ;; (print all)
    all))


(comment
  (def info (parse day-5-input))
  (def seeds (first info))
  (def seeds' (expand seeds))
  (def maps (second info))

  (second info)
  ;;
  )

(defn location2 [maps seed]
  (let [m-f (memoize
             (fn [seed]
               (location maps seed)))]
    (m-f seed)))

(def location3 (memoize location))

(defn lowest-location-2 [input]
  (let [info (parse input)
        seeds (first info)
        seeds' (expand seeds)
        maps (second info)]
    (apply min (map (fn [seed]
                      (location2 maps seed)) seeds'))))



(comment

  (def totalseedss
    (apply + (map second (partition 2 (first (parse day-5-input))))))
  (def days-to-complete (/ (/ (/ (* 0.001 totalseedss) 60) 60) 24))

  (println "There are totally" (/ totalseedss 1000000.0) "millions seeds\n"
           "It will take" days-to-complete "days")

  (second (parse day-5-input))

  (time (lowest-location-2 input))




  (lowest-location-2 day-5-input)
  ;
  )

(is (= 46 (lowest-location-2 input)))

