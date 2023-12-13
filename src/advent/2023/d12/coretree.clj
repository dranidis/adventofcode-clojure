(ns advent.2023.d12.coretree
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]))

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

(defn is-partial-valid? [springs numbers]
  ;; (prn "---> IS partial valid?" springs numbers)
  (let [springs-list (re-seq #"[#?]+" springs)
        ans
        (loop [i 0
               valid? true
               break? false]
          (if (or break? (>= i (count springs-list)) (>= i (count numbers)))
            valid?
            (let [s (nth springs-list i)
                  remove-trail-?-s (str/replace s #"\?*$" "")]
              (if (= "" remove-trail-?-s) ; only ????
                true
                (let [only-#-? (= "" (str/replace remove-trail-?-s "#" ""))]
                  (if only-#-?
                    (if (<= (count remove-trail-?-s) (nth numbers i)) ; less= ### than number
                      (if (= remove-trail-?-s s) ;; not removed any ?
                        (if (= (count s) (get numbers i)) ; exactly ###
                          (recur (inc i) true false)
                          false)
                        true)
                      false)
                    true))))))]
    (println springs numbers ans)
    ans))

(defn generate-tree [s prefix numbers]
  (prn "generate-tree" s prefix numbers (str/index-of s "?"))
  (if-let [next-?-pos (str/index-of s "?")]
    (let [str-till-? (subs s 0 next-?-pos)
          str-after-? (subs s (inc next-?-pos))
          expr (str prefix str-till-?)
          next-hash-str (str "#" str-after-?)
          next-dot-str (str "." str-after-?)
          valid-hash? (is-partial-valid? (str expr next-hash-str)  numbers)
          valid-dot? (is-partial-valid? (str expr next-dot-str)  numbers)]
      (println "RET [" str-till-?  " [ " next-hash-str next-dot-str "] ]")
      [str-till-?
       (if valid-hash? (generate-tree next-hash-str expr numbers)
           nil)
       (if valid-dot? (generate-tree next-dot-str expr numbers)
           nil)])
    s))


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

(prune (generate-tree ".??..??...?##." "" [1 1 3]) 5)

(comment
  `(generate-tree ".??..??...?##.")
  (generate-tree "###?...")
  (prune (generate-tree "#?#") 2)
  (prune (generate-tree "..?...?...") 2)

  (def t (generate-tree ".??..??...?##." "" [1 1 2]))
  (tree-seq seq? identity t)
  (map first (tree-seq next rest t))
  ;
  )

(pprint/pp)

