(ns advent.2020.d18.core
  (:require
   [clojure.string :as str]))

(defn line-p [line]
  (->> line
       (re-seq #"[\d+\+\*\(\)]{1}")
       (mapv (fn [s] (if-let [n (parse-long s)] n s)))))

(def input-expressions (->> (slurp "src/advent/2020/d18/input.txt")
                            str/split-lines
                            (mapv line-p)))

(defn pars->vec [expression expr-with-vectors]
  (if (empty? expression)
    expr-with-vectors
    (let [symbol (first expression)]
      (if (not= ")" symbol)
        (pars->vec (rest expression) (conj expr-with-vectors symbol))

        (let [n-expr-with-vectors
              (loop [stck expr-with-vectors
                     lifo (list)]
                  ;; (println "L" stck e)
                (if (empty? stck)
                  :error
                  (let [top (peek stck)]
                    (if (not= top "(")
                      (recur (pop stck) (conj lifo top))
                      (conj (pop stck) lifo)))))]
          (pars->vec (rest expression) n-expr-with-vectors))))))

(defn evaluate-vec-expression
  [expression]
  (letfn
   [(evl [pe nums op]
      (if (empty? pe)
        (if (empty? op)
          (first nums)
          (loop [nums nums
                 op op]
            (if (empty? op)
              (first nums)
              (let [f (first nums)
                    s (second nums)
                    r (if (= "+" (first op)) (+ f s) (* f s))]
                (recur (apply conj [r] (drop 2 nums))
                       (drop 1 op))))))
        (let [elem (nth pe 0)]
          (cond
            (#{"+" "*"} elem)
            (evl (rest pe) nums (conj op elem))

            (number? elem)
            (evl (rest pe) (conj nums elem) op)

            :else ;; collection
            (let [elem (evl elem [] [])]
              (evl (rest pe) (conj nums elem) op))))))]
    (evl expression [] [])))

;; part 1
(defn compute [expression]
  (-> expression
      (pars->vec [])
      evaluate-vec-expression))


(->> input-expressions
     (map compute)
     (apply +)
     println)

;; part 2
(defn post-fix-calc [post-fix-expr]
  (loop [outp post-fix-expr
         stack []]
    (if (empty? outp)
      (peek stack)
      (let [sym (first outp)]
        (if (number? sym)
          (recur (rest outp) (conj stack sym))

          (let [f (peek stack)
                s (peek (pop stack))
                res
                ((read-string sym) f s)
                ;; (if (= "+" sym) (+ f s) (* f s))
                ]
            (recur (rest outp) (-> stack pop pop (conj res)))))))))

(defn- evaluate-vec-expression-2 [expression]
  (letfn
   [(evl [pe nums ops]
      (if (empty? pe)
        (post-fix-calc
         (loop [nums nums
                ops ops]
           (if (empty? ops)
             nums
             (recur (conj nums (peek ops)) (pop ops)))))

        (let [elem (nth pe 0)]
          (cond
            (= elem "+") ;; has higher prec than "*" 
            (evl (rest pe) nums (conj ops elem))

            (= elem "*") ;; has lower prec than "+"; pop all "+" and push to the out
            (let [[nums ops] (loop [nums nums
                                    ops ops]
                               (if (empty? ops)
                                 [nums ops]
                                 (let [top (peek ops)]
                                   (if (= top "+")
                                     (recur (conj nums top) (pop ops))
                                     [nums ops]))))]
              (evl (rest pe) nums (conj ops elem)))

            (number? elem)
            (evl (rest pe) (conj nums elem) ops)

            :else ;; collection
            (let [elem (evl elem [] [])]
              (evl (rest pe) (conj nums elem) ops))))))]
    (evl expression [] [])))

(defn compute2 [expression]
  (-> expression
      (pars->vec [])
      evaluate-vec-expression-2))

(->> input-expressions
     (map compute2)
     (apply +)
     println)