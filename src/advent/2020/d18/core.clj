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
                res (if (= "+" sym) (+ f s) (* f s))]
            (recur (rest outp) (-> stack pop pop (conj res)))))))))

(defn- evaluate-vec-expression-2 [precedence expression]
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
            ;; (= elem "+") ;; has higher prec than "*" 
            ;; (evl (rest pe) nums (conj ops elem))

            (#{"*" "+"} elem) ;; has lower prec than "+"; pop all "+" and push to the out
            (let [[nums ops] (loop [nums nums
                                    ops ops]
                               (if (empty? ops)
                                 [nums ops]
                                 (let [top (peek ops)]
                                   (if (>= (precedence top elem) 0)
                                     (recur (conj nums top) (pop ops))
                                     [nums ops]))))]
              (evl (rest pe) nums (conj ops elem)))

            (number? elem)
            (evl (rest pe) (conj nums elem) ops)

            :else ;; collection
            (let [elem (evl elem [] [])]
              (evl (rest pe) (conj nums elem) ops))))))]
    (evl expression [] [])))

(defn compute1 [expression]
  (-> expression
      (pars->vec [])
      ((fn [e] (evaluate-vec-expression-2
                (fn [a b] 0) ;; use same precedenct for + and * 
                e)))))

(->> input-expressions
     (map compute1)
     (apply +)
     println)

;; part 2

(defn precedence-plus [sym1 sym2]
  (cond (= sym1 sym2) 0
        (= sym1 "+") 1 ;; use higher precedenct for + over *
        :else -1))

(defn compute2 [expression]
  (-> expression
      (pars->vec [])
      ((fn [e] (evaluate-vec-expression-2 precedence-plus e)))))

(->> input-expressions
     (map compute2)
     (apply +)
     println)