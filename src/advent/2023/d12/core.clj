(ns advent.2023.d12.core
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]))

;; (def example (slurp "src/advent/2023/d12/example.txt"))
(def input (slurp "src/advent/2023/d12/input.txt"))

(def input "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn- parse-line [line]
  (let [[springs numbers] (str/split line #"  *")
        numbers (read-string (str "[" (str/replace numbers #"," " ") "]"))]
    [springs numbers]))

(defn parse [input]
  (vec (for [line (str/split-lines input)]
         (parse-line line))))

;; (str/index-of springs \?)

;; ;; find all indices of ?
;; ;; a ? is either "." or "#"



;; (def indices [0 1 2]) ; for ???.###
;; (for [i indices
;;       repl ["#" "."]]
;;   (loop [s springs
;;          i 0]
;;     ((str/replace-first s "?" repl))))

;; (def i 0)
;; (def substr (subs springs i))

(defn replace-xx [springs i]
  (let [start-str (subs springs 0 i)
        end-str (subs springs i)
        repls (mapv (fn [r]
                      (str start-str (str/replace-first end-str "?" r)))
                    ["#" "."])]
    repls))

(comment
  (def springs-str ".??..??...?##.")

  (def pi (parse input))
  (first pi)
  (def springs (first (second pi)))
  (def numbers (second (second pi)))
  ;
  )


(comment
  (def springs "#.??.######..#####.")
  (def numbers [1 6 5])
  (generate-all springs numbers)
  ;; (generate-all-1 springs numbers)

  (def s "#")
  (def n 1)
  (def len 2)

  (def s "#?")
  (def n 1)
  (def len 2)

  ;
  )

(defn- if-only-#-correct-size? [s n]
  (let [len (count s)]
    (or (> (count (str/replace s "#" "")) 0)
        (= n len))))

(comment
  (def springs ".#..###.??.###???.###???.###???.###")
  (def numbers [1 1 3 1 1 3 1 1 3 1 1 3 1 1 3])
  (def springs-list (re-seq #"[#?]+" springs))
  (def i 1)
  (def s (nth springs-list i))
  ;
  )
(defn is-partial-valid? [springs numbers]
  ;; (prn "IS partial valid?" springs numbers)
  (let [springs-list (re-seq #"[#?]+" springs)
        ans
        ;; (every? true? (map if-only-#-correct-size? springs-list numbers))
        (loop [i 0
               valid? true
               break? false]
          ;; (prn i valid? break?)
          (if (or break? (>= i (count springs-list)))
            valid?
            (let [s (nth springs-list i)
                  only-#-? (= "" (str/replace s "#" ""))]
              (if only-#-?
                (if (= (count s) (get numbers i))
                  (recur (inc i) true false)
                  (recur (inc i) false true))
                (recur (inc i) true true)))))]
    ;; (println springs numbers ans)
    ans))


(is (not (is-partial-valid? ".##..??...?##." [1 1 3])))
(is (not (is-partial-valid? ".##?.??...?##." [1 1 3])))

(is (is-partial-valid? ".#?..??...?##." [1 1 3]))
(is (is-partial-valid? "#.??.######..#####." [1 6 5]))
(is (not (is-partial-valid? ".#..###.??.###???.###???.###???.###" [1 1 3 1 1 3 1 1 3 1 1 3 1 1 3])))

(defn generate-all [springs numbers]
  ;; (prn springs numbers)
  (let [len (count springs)]
    (loop [i 0
           replacements #{}
           cnt 0]
      ;; (when (zero? (mod cnt 10))
      ;;   (prn cnt springs (count replacements)))
      (if (>= i len)
        replacements
        (do
          ;; (prn i  (get springs i))
          (if (not= \? (get springs i))
            (recur (inc i) replacements cnt)
            (do
              ;; (prn i  (get springs i) replacements)
              (if (empty? replacements)
                (recur (inc i) (set (replace-xx springs 0)) (inc cnt))
                (let [replacements
                      (set (flatten
                            (mapv (fn [r]
                                    (filter
                                     (fn [s] (is-partial-valid? s numbers))
                                     (replace-xx r i)))
                                  replacements)))]
                  (recur (inc i) replacements (inc cnt))))))))))
                  ;
  )


(defn generate-all-1 [springs numbers]
  (let [len (count springs)]
    (loop [i 0
           replacements #{}
           cnt 0]
      ;; (when (zero? (mod cnt 10))
      ;;   (prn cnt springs (count replacements)))
      (prn i replacements)
      (if (>= i len)
        replacements
        (if (not= \? (get springs i))
          (recur (inc i) replacements cnt)
          (if (empty? replacements)
            ;; (if (not= i 0)
            ;;   replacements
            (recur (inc i) (set (replace-xx springs 0)) (inc cnt))
            ;; )
            (let [replacements
                  (set (flatten
                        (mapv (fn [r]
                                (filter
                                 (fn [s] true)
                                 (replace-xx r i)))
                              replacements)))]
              (recur (inc i) replacements (inc cnt))))))))
                  ;
  )

;; (defn generate-all-1 [springs-str numbers]
;;   (let [len (count springs-str)]
;;     (loop [i 0
;;            replacements #{}
;;            cnt 0]
;;       (when (zero? (mod cnt 10))
;;         (prn cnt springs-str (count replacements)))
;;       (if (>= i len)
;;         replacements
;;         (if (not= \? (get springs-str i))
;;           (recur (inc i) replacements cnt)
;;           (if (empty? replacements)
;;             (recur (inc i) (set (replace-xx springs-str 0)) (inc cnt))
;;             (let [replacements (set (flatten (mapv (fn [r]
;;                                                      (replace-xx r i))
;;                                                    replacements)))]
;;               (recur (inc i) replacements (inc cnt)))))))))



(defn is-valid? [springs numbers]
  (let [;; springs-list (str/split springs #"\.")
        springs-list (re-seq #"#+" springs)]
    (and (= (count springs-list) (count numbers))
         (every? true? (map (fn [s n] (= n (count s))) springs-list numbers)))))

(is (is-valid? "#.#.###" [1 1 3]))
(is (not (is-valid? "##..###" [1 1 3])))
(is (not (is-valid? "..#...#....##." [1 1 3])))
(is (is-valid? "..#..#....###." [1 1 3]))

(apply generate-all (parse-line "????.######..#####. 1,6,5"))
;; (apply generate-all-1 (parse-line "????.######..#####. 1,6,5"))


(defn- arrangements [[springs numbers]]
  (vec (distinct (filterv (fn [s] (is-valid? s numbers)) (generate-all springs numbers)))))


(defn- arrangements-1 [[springs numbers]]
  (vec (distinct (filterv (fn [s] (is-valid? s numbers)) (generate-all-1 springs numbers)))))

(is (= ["#.#.###"] (arrangements (parse-line "???.### 1,1,3"))))

(is (= (set [".#...#....###."
             "..#..#....###."
             "..#...#...###."
             ".#....#...###."]) (set (arrangements-1 (parse-line ".??..??...?##. 1,1,3")))))

(is (= (set [".#...#....###."
             "..#..#....###."
             "..#...#...###."
             ".#....#...###."]) (set (arrangements (parse-line ".??..??...?##. 1,1,3")))))

(is (= (set [".###.##.#..."
             ".###.##..#.."
             ".###.##...#."
             ".###.##....#"
             ".###..##.#.."
             ".###..##..#."
             ".###..##...#"
             ".###...##.#."
             ".###...##..#"
             ".###....##.#"]) (set (arrangements (parse-line "?###???????? 3,2,1")))))

(is (= #{"#....######..#####."
         "...#.######..#####."
         "..#..######..#####."
         ".#...######..#####."}
       (set (arrangements (parse-line "????.######..#####. 1,6,5")))))

(arrangements ["???.###???.###???.###???.###???.###" [1 1 3 1 1 3 1 1 3 1 1 3 1 1 3]])
(arrangements-1 ["???.###???.###???.###???.###???.###" [1 1 3 1 1 3 1 1 3 1 1 3 1 1 3]])



;; (defn diff [input]
;;   (map (fn [s] (is (= (set (arrangements s)) (set (arrangements-1 s)))))
;;        (parse input)))
;; (diff input)

(defn answer-1 [input]
  (apply + (map (fn [s] (count (arrangements s)))
                (parse input))))

;; (is (= 21 (answer-1 input)))

;; "Elapsed time: 31.456501 msecs"
;; "Elapsed time: 7.778614 msecs"
;; 21
;; 
;; 7221
;; "Elapsed time: 227019.626682 msecs"
;; 7221
;; "Elapsed time: 58426.406953 msecs" PC
;; 7221
;; "Elapsed time: 20303.681225 msecs" Laptop
;; 7221
;; "Elapsed time: 2009.613262 msecs"

;; (time (println (answer-1 input)))
;; part 2


(defn- parse-line-2 [l]
  (let [[springs numbers] (parse-line l)]
    [(str springs "?" springs "?" springs "?" springs "?" springs)
     (vec (concat numbers numbers numbers numbers numbers))]))

(defn parse-2 [input]
  (vec (for [line (str/split-lines input)]
         (parse-line-2 line))))

(defn answer-2 [input]
  (apply + (map (fn [s] (count (arrangements s)))
                (parse-2 input))))

(comment
  (arrangements (nth (parse input) 0))
  (arrangements (nth (parse-2 input) 0))
  ;
  )

;; (time (println (answer-2 input)))

;; 102050 Example input right now is wrong
(defn -main [& _]
  (time (println (answer-2 input))))

(comment
  (map (fn [s] (count (arrangements s)))
       (parse-2 input))

  1
  16384
  1
  16
  2500
  506250
  ;
  )

;; "Elapsed time: 7450.651682 msecs"
(time (let [input "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"]
        (is (= 1 (count (arrangements (nth (parse-2 input) 0)))))
        (is (= 16384 (count (arrangements (nth (parse-2 input) 1)))))
        (is (= 1 (count (arrangements (nth (parse-2 input) 2)))))
        (is (= 16 (count (arrangements (nth (parse-2 input) 3)))))
        (is (= 2500 (count (arrangements (nth (parse-2 input) 4)))))))