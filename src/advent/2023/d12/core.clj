(ns advent.2023.d12.core
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]))

(def example (slurp "src/advent/2023/d12/example.txt"))
(def input (slurp "src/advent/2023/d12/input.txt"))

;; (def input "???.### 1,1,3
;; .??..??...?##. 1,1,3
;; ?#?#?#?#?#?#?#? 1,3,1,6
;; ????.#...#... 4,1,1
;; ????.######..#####. 1,6,5
;; ?###???????? 3,2,1")

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

(defn is-partial-valid? [springs numbers]
  (let [;; springs-list (str/split springs #"\.")
        springs-list (re-seq #"[#\?]+" springs)]
    (and (= (count springs-list) (count numbers))
         (every? true? (map (fn [s n] (= n (count s))) springs-list numbers)))))

(defn generate-all [springs]
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
                (let [replacements (set (flatten (mapv (fn [r]
                                                         (replace-xx r i))
                                                       replacements)))]
                  (recur (inc i) replacements (inc cnt)))))))))))

(comment
  (generate-all springs)

  (def springs "..#...#....##.")
  ;
  )

(defn is-valid? [springs numbers]
  (let [;; springs-list (str/split springs #"\.")
        springs-list (re-seq #"#+" springs)]
    (and (= (count springs-list) (count numbers))
         (every? true? (map (fn [s n] (= n (count s))) springs-list numbers)))))

(is (is-valid? "#.#.###" [1 1 3]))
(is (not (is-valid? "##..###" [1 1 3])))
(is (not (is-valid? "..#...#....##." [1 1 3])))
(is (is-valid? "..#..#....###." [1 1 3]))

(defn- arrangements [[springs numbers]]
  (vec (distinct (filterv (fn [s] (is-valid? s numbers)) (generate-all springs)))))

(is (= ["#.#.###"] (arrangements (parse-line "???.### 1,1,3"))))

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

(defn answer-1 [input]
  (apply + (map (fn [s] (count (arrangements s)))
                (parse input))))


;; "Elapsed time: 31.456501 msecs"
;; "Elapsed time: 7.778614 msecs"
;; 21
;; 
;; 7221
;; "Elapsed time: 227019.626682 msecs"
;; 7221
;; "Elapsed time: 58426.406953 msecs"
(time (println (answer-1 input)))
;; part 2


(defn- parse-line-2 [l]
  (let [[springs numbers] (parse-line l)]
    [(str springs springs springs springs springs)
     (vec (concat numbers numbers numbers numbers numbers))]))

(defn parse-2 [input]
  (vec (for [line (str/split-lines input)]
         (parse-line-2 line))))

(defn answer-2 [input]
  (apply + (map (fn [s] (count (arrangements s)))
                (parse-2 input))))

;; (time (println (answer-2 input)))