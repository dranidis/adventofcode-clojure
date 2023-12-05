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


(def pers-maps (atom {}))

(declare ranges-map)

(defn produce-maps-for-key [maps map-key]
  (let [ranges (:ranges (first (get maps map-key)))
        the-map (reduce (fn [acc r]
                          (let [start (second r)
                                end (+ start (nth r 2))]
                            (loop [i start
                                   hashmap acc]
                              (if (>= i end)
                                hashmap
                                (recur (inc i) (assoc hashmap i (ranges-map [r] i)))))))
                        {}
                        ranges)]
    the-map))

(defn update-maps! [maps]
  (let [_ (reset! pers-maps {})
        seed (produce-maps-for-key maps "seed")
        soil (produce-maps-for-key maps "soil")
        fertilizer (produce-maps-for-key maps "fertilizer")
        water (produce-maps-for-key maps "water")
        light (produce-maps-for-key maps "light")
        temperature (produce-maps-for-key maps "temperature")
        humidity (produce-maps-for-key maps "humidity")
        location (produce-maps-for-key maps "location")]
    (swap! pers-maps assoc "seed" seed)
    (swap! pers-maps assoc "soil" soil)
    (swap! pers-maps assoc "fertilizer" fertilizer)
    (swap! pers-maps assoc "water" water)
    (swap! pers-maps assoc "light" light)
    (swap! pers-maps assoc "temperature" temperature)
    (swap! pers-maps assoc "humidity" humidity)
    (swap! pers-maps assoc "location" location)))

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
         maps (group-by :source (map parse-mapping maps-string))
        ;;  _ (update-maps!  maps)
         ]
    [seeds maps]))

(comment
  (parse input)
  ;
  )

(defn ranges-map [ranges id]
  (let [l (filter (fn [r] (<= (second r) id (+ (second r) (nth r 2)))) ranges)]
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

(comment
  (defn map-id->id [maps map-key an-id]
    (ranges-map (:ranges (first (get maps map-key))) an-id))
  ;
  )

(defn map-id->id [maps map-key an-id]
  (let [result (get-in @pers-maps [map-key an-id])]
    (if (nil? result)
      an-id
      result)))

(comment

  {65 67, 70 72, 62 64, 74 76, 59 61, 86 88, 72 74, 58 60, 60 62, 69 71, 55 57, 85 87, 88 90, 77 79, 95 97, 54 56, 92 94, 50 52, 75 77, 99 51, 91 93, 56 58, 90 92, 89 91, 61 63, 93 95, 64 66, 51 53, 66 68, 82 84, 76 78, 97 99, 57 59, 68 70, 83 85, 53 55, 78 80, 81 83, 79 81, 98 50, 87 89, 73 75, 96 98, 52 54, 67 69, 71 73, 80 82, 63 65, 94 96, 84 86}


  (map-id->id (second (parse input)) "seed" 79)
  ;
  )



(comment
  (def maps (second (parse input)))
  (def seed-id 79)
  ;
  )

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

;; (is (= 313045984 (lowest-location day-5-input)))
(is (= 35 (lowest-location input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; PART 2
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn lowest-location-2 [input]
  (let [info (parse input)
        seeds (first info)
        seeds' (expand seeds)
        maps (second info)]
    (apply min (map (fn [seed]
                      (location maps seed)) seeds'))))

(comment
  (def totalseedss
    (apply + (map second (partition 2 (first (parse day-5-input))))))
  (def days-to-complete (/ (/ (/ (* 0.001 totalseedss) 60) 60) 24))

  (println "There are totally" (/ totalseedss 1000000.0) "millions seeds\n"
           "It will take" days-to-complete "days")

  (time (lowest-location-2 input))


  (lowest-location-2 day-5-input)

  (time (lowest-location-2 input)))

;; (is (= 46 (lowest-location-2 input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; try solution with intersections
;;
;; seed-soil                 [50                   97] [98 99]
;;                            +2                      -48
;;                           [52                   99] [50 51]
;; soil-fert [0  14] [15 51] [52 53]
;; 
;;           [39 53] [0  36] [37 38]            

;; 

(defn source [r] (second r))
(defn dest [r] (first r))
(defn len [r] (last r))

;; ;; seed ranges
;; (sort (map (fn [[s l]] [s (+ s (dec l))]) (partition 2 (first (parse input)))))

;; ;; seed to soil ranges
;; (sort (map (fn [r]
;;              [(source r) (+ (source r) (dec (len r)))])
;;            (get-in (second (parse input)) ["soil" 0 :ranges])))

;; (sort-by first (map (fn [s] [(first s) (second s)]) (map (fn [r]
;;                                                            [[(source r) (+ (source r) (dec (len r)))] [(dest r) (+ (dest r) (dec (len r)))]])
;;                                                          (get-in (second (parse input)) ["soil" 0 :ranges]))))




;; GO THROUGH ALL LOCATIONS

(defn- inv-ranges-map [ranges id]
  (let [l (filter (fn [r] (<= (dest r) id (+  (dest r) (dec (len r)))))
                  ranges)]
    (if (empty? l)
      id
      (let [range (first l)]
        (+ (- id (dest range))  (source range))))))

(is (= 79 (inv-ranges-map [[50 98 2] [52 50 48]] 81)))
(is (= 14 (inv-ranges-map [[50 98 2] [52 50 48]] 14)))
(is (= 55 (inv-ranges-map [[50 98 2] [52 50 48]] 57)))
(is (= 13 (inv-ranges-map [[50 98 2] [52 50 48]] 13)))

(defn- in-ranges? [s seed]
  (some #(<= (first %) seed (+ (first %) (dec (second %))))
        (partition 2 s)))

(defn ans2 [input]
  (let [pi (parse input)
        s (first pi)
        m (second pi)]
    (loop [location 0]
      (let [seed (->> location
                      (inv-ranges-map (get-in m ["humidity" 0 :ranges]))
                      (inv-ranges-map (get-in m ["temperature" 0 :ranges]))
                      (inv-ranges-map (get-in m ["light" 0 :ranges]))
                      (inv-ranges-map (get-in m ["water" 0 :ranges]))
                      (inv-ranges-map (get-in m ["fertilizer" 0 :ranges]))
                      (inv-ranges-map (get-in m ["soil" 0 :ranges]))
                      (inv-ranges-map (get-in m ["seed" 0 :ranges])))
            seed-in-ranges? (in-ranges? s seed)]
        (when (= (mod location 100000) 0)
          (println location))
        (if seed-in-ranges?
          location
          (recur (inc location)))))))

(ans2 input)
(ans2 day-5-input)
