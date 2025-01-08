(ns advent.2022.d25.core
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]))

;; (def input "1=-0-2
;; 12111
;; 2=0=
;; 21
;; 2=01
;; 111
;; 20012
;; 112
;; 1=-1=
;; 1-12
;; 12
;; 1=
;; 122")

(def input (slurp "src/advent/2022/d25/input.txt"))

(def snafus (->> input str/split-lines))

(defn snafu->dec [s]
  (let [ss (str/split s #"")]
    (loop [ss ss
           n 0
           p5 1]
      (if (empty? ss)
        n
        (let [f (peek ss)
              f (case f  "-" -1 "=" -2 (parse-long f))
              n (+ n (* f p5))
              p5 (* 5 p5)]
          (recur (pop ss) n p5))))))

(defn snafu->arr [s]
  (mapv (fn [f] (case f  "-" -1 "=" -2 (parse-long f)))
        (str/split s #"")))

(defn to- [n]
  (let [quot-5 (quot n 5)
        mod-5 (mod n 5)
        m (if (> mod-5 2)
            [1 (- mod-5 5)]
            mod-5)
        nq (if (vector? m) (+ quot-5 (first m)) quot-5)
        nm (if (vector? m) (peek m) m)]

    (if (> nq 2)
      (conj (to- nq) nm)
      (if (zero? nq) [nm]
          [nq nm]))))

(is (= [2] (to- 2)))
(is (= [1 -2] (to- 3)))
(is (= [1 -1] (to- 4)))
(is (= [1 0] (to- 5)))
(is (= [1 1] (to- 6)))
(is (= [1 2] (to- 7)))
(is (= [2 -2] (to- 8)))
(is (= [2 -1] (to- 9)))
(is (= [2 0] (to- 10)))
(is (= [2 2] (to- 12)))
(is (= [1 -2 -2] (to- 13)))
(is (= [1 -2 -1] (to- 14)))
(is (= [1 -2 0] (to- 15)))
(is (= (snafu->arr "1121-1110-1=0") (to- 314159265)))

(defn arr->snafu [arr]
  (apply str (map (fn [x] (case x -1 "-" -2 "=" (str x))) arr)))

(println (arr->snafu (to-
                      (apply + (mapv snafu->dec snafus)))))




