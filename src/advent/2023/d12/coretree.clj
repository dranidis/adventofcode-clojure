(ns advent.2023.d12.coretree
  (:require [clojure.string :as str]
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

(def mm (atom {}))

(defn valid-part? [s n]
  (let [m-f (memoize
             (fn [s n]
               (if (get-in @mm [s n])
                 (get-in @mm [s n])
                 (let [v? (let [remove-trail-?-s (str/replace s #"\?*$" "")]
                            (or (= "" remove-trail-?-s)                            ; only ???s
                                (if (= "" (str/replace remove-trail-?-s "#" ""))  ;; only ###s
                                  (if (<= (count remove-trail-?-s) n) ; less= ### than number
                                    (if (= remove-trail-?-s s) ;; not removed any ?
                                      (if (= (count s) n) ; exactly ###
                                        :recur
                                        false)
                                      true)
                                    false)
                                  true)))]
                   (swap! mm assoc-in [s n] v?)
                   v?))))]
    (m-f s n)))

(defn is-partial-valid? [springs numbers]
  ;; (prn "IS partial valid?" springs numbers)
  (let [springs-list (re-seq #"[#?]+" springs)
        ans
        (loop [i 0
               valid? true
               break? false]
          (if (or break? (>= i (count springs-list)) (>= i (count numbers)))
            valid?
            (let [v? (valid-part? (nth springs-list i) (nth numbers i))]
              (if (= :recur v?)
                (recur (inc i) true false)
                v?))))]
    ;; (println springs numbers ans)
    ans))

(defn is-valid? [expr numbers]
  (let [;; springs-list (str/split springs #"\.")
        springs-list (re-seq #"#+" expr)]
    (and (= (count springs-list) (count numbers))
         (every? true? (map (fn [s n] (= n (count s))) springs-list numbers)))))

(defn generate-tree [s prefix numbers]
  ;; (prn "generate-tree" s prefix numbers (str/index-of s "?"))
  (if-let [next-?-pos (str/index-of s "?")]
    (let [str-till-? (subs s 0 next-?-pos)
          str-after-? (subs s (inc next-?-pos))
          expr (str prefix str-till-?)
          next-hash-str (str "#" str-after-?)
          next-dot-str (str "." str-after-?)
          valid-hash? (is-partial-valid? (str expr next-hash-str)  numbers)
          valid-dot? (is-partial-valid? (str expr next-dot-str)  numbers)]
      ;; (println "RET [" str-till-?  " [ " next-hash-str next-dot-str "] ]")
      [str-till-?
       (if valid-hash? (generate-tree next-hash-str expr numbers)
           nil)
       (if valid-dot? (generate-tree next-dot-str expr numbers)
           nil)])
    (if (is-valid? (str prefix s) numbers)
      s
      nil)))

(def cnt (atom 1))

(defn gen-tree [s numbers]
  (swap! cnt inc)
  (println @cnt s numbers)
  (let [start# (. System (nanoTime))
        t (generate-tree s "" numbers)]
    (println (/ (double (- (. System (nanoTime)) start#)) 1000000000.0) "secs")
    t))

(defn prune [tree total]
  (cond
    (vector? tree)
    (let [root (first tree)
          left (second tree)
          right (nth tree 2)
          count#s (count (filter (fn [s] (= s \#)) root))]
      (if (< count#s total)
        [root
         (prune left (- total count#s))
         (prune right (- total count#s))]
        root))
    :else (if (= (count (filter (fn [s] (= s \#)) tree)) total)
            tree nil)))


(defn count-leaves [tree]
  (cond
    (not (vector? tree)) (if-not (nil? tree) 1 0)
    :else (+ (count-leaves (second tree)) (count-leaves (nth tree 2)))))


(defn answer-1 [input]
  (apply + (map (fn [[i n]]
                  (count-leaves (gen-tree i n)))
                (parse input))))

;; "Elapsed time: 1632.635265 msecs"
;; 7221
;; (time (answer-1 (slurp "src/advent/2023/d12/input.txt")))

(defn- parse-line-2 [l]
  (let [[springs numbers] (parse-line l)]
    [(str springs "?" springs "?" springs "?" springs "?" springs)
     (vec (concat numbers numbers numbers numbers numbers))]))

(defn parse-2 [input]
  (vec (for [line (str/split-lines input)]
         (parse-line-2 line))))

(defn answer-2 [input]
  (apply + (map (fn [[i n]]
                  (count-leaves (gen-tree i n)))
                (parse-2 input))))

(comment
;; "Elapsed time: 9135.826422 msecs"
  (time (let [input "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"]
          (is (= 1 (count-leaves (apply gen-tree (nth (parse-2 input) 0)))))
          (is (= 16384 (count-leaves (apply gen-tree (nth (parse-2 input) 1)))))
          (is (= 1 (count-leaves (apply gen-tree (nth (parse-2 input) 2)))))
          (is (= 16 (count-leaves (apply gen-tree (nth (parse-2 input) 3)))))
          (is (= 2500 (count-leaves (apply gen-tree (nth (parse-2 input) 4)))))))
  ;      
  )
;; (time (answer-2 input))

(defn -main [& _]
  (println  (count (parse (slurp "src/advent/2023/d12/input.txt"))))
  (time (println (answer-2 (slurp "src/advent/2023/d12/input.txt")))))

