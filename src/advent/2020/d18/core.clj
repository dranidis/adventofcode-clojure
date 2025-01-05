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

(defn- shunting-alg [precedence infix-expression]
  (letfn
   [(evl [infix-expression output op-stack]
      (if (empty? infix-expression)
        (loop [output output
               op-stack op-stack]
          (if (empty? op-stack)
            output
            (recur (conj output (peek op-stack)) (pop op-stack))))

        (let [elem (nth infix-expression 0)]
          (cond
            (number? elem) (evl (rest infix-expression) (conj output elem) op-stack)

            (= elem "(")   (evl (rest infix-expression) output (conj op-stack elem))

            ;; while there is an operator at the top of the operator stack 
            ;; which is not a left parenthesis, 
            ;; and with greater or (equal precedence if is left-associative))
            (#{"*" "+"} elem) (let [[output op-stack]
                                    (loop [output output
                                           op-stack op-stack]
                                      (if (empty? op-stack)
                                        [output op-stack]
                                        (let [top (peek op-stack)]
                                          (if (= top "(")
                                            [output op-stack]
                                            (if (>= (precedence top elem) 0)
                                              (recur (conj output top) (pop op-stack))
                                              [output op-stack])))))]
                                (evl (rest infix-expression) output (conj op-stack elem)))

            (= elem ")") (let [[output op-stack]
                               (loop [output output
                                      op-stack op-stack]
                                 (assert (seq op-stack))
                                 (let [op (peek op-stack)]
                                   (if (= op "(")
                                     [output (pop op-stack)]
                                     (recur (conj output op) (pop op-stack)))))]
                           (evl (rest infix-expression) output op-stack))

            :else :error))))]
    (evl infix-expression [] [])))

;; part 2
(defn compute1 [expression]
  (-> expression
      ((fn [e] (shunting-alg
                (fn [_ _] 0) ;; use same precedenct for + and * 
                e)))
      post-fix-calc))

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
  (->> expression
       ((partial shunting-alg precedence-plus))
       post-fix-calc))

(->> input-expressions
     (map compute2)
     (apply +)
     println)