(ns advent.2023.d19-test
  (:require [advent.2023.d19.core :refer :all]
            [clojure.test :refer [deftest is]]))
(deftest p-1-2
  (let [ind-rules (first (parse-2 input))]
    (is (= [1 99] (get-interval ["<" "x" 100])))
    (is (= [101 max-num] (get-interval [">" "x" 100])))
    (is (= [100 max-num] (get-inverse-interval ["<" "x" 100])))
    (is (= [1 100] (get-inverse-interval [">" "x" 100])))

    (is (= [5 50] (interval-intersection [1 100] [5 50])))
    (is (= [5 50] (interval-intersection [5 50] [1 100])))
    (is (= [50 100] (interval-intersection [1 100] [50 150])))
    (is (= nil (interval-intersection [1 100] [105 150])))
    (is (= [50 100] (interval-intersection [1 100] [50 150])))
    (is (= [2771 3448] (interval-intersection [2771 4000] [1 3448])))
    (is (= [2771 3448] (interval-intersection [1 3448] [2771 4000])))

    (is (= (set [{:rule :gd,
                  :intervals
                  {"x" [1 4000], "m" [1 2090], "a" [2006 4000], "s" [1 536]}}
                 {:rule :R,
                  :intervals
                  {"x" [2441 4000], "m" [1 2090], "a" [2006 4000], "s" [537 1350]}}
                 {:rule :A,
                  :intervals
                  {"x" [1 2440], "m" [1 2090], "a" [2006 4000], "s" [537 1350]}}])
           (set (process-interval ind-rules
                                  {:rule :rfg,
                                   :intervals
                                   {"x" [1 4000],
                                    "m" [1 2090],
                                    "a" [2006 4000],
                                    "s" [1 1350]}}))))

    (is (= (set [{:rule :gd,
                  :intervals
                  {"x" [100 3000], "m" [200 2000], "a" [300 1000], "s" [400 536]}}
                 {:rule :R,
                  :intervals
                  {"x" [2441 3000], "m" [200 2000], "a" [300 1000], "s" [537 900]}}
                 {:rule :aa,
                  :intervals
                  {"x" [100 2440], "m" [451 2000], "a" [300 1000], "s" [537 900]}}
                 {:rule :ab,
                  :intervals
                  {"x" [100 2440], "m" [200 450], "a" [300 499], "s" [537 900]}}
                 {:rule :A,
                  :intervals
                  {"x" [100 2440], "m" [200 450], "a" [500 1000], "s" [537 900]}}])
           (set (process-interval (assoc ind-rules
                                         :rfg
                                         {:rule :rfg, :conds [["<" "s" 537 :gd] [">" "x" 2440 :R]
                                                              [">" "m" 450 :aa] ["<" "a" 500 :ab]], :else :A})
                                  {:rule :rfg,
                                   :intervals
                                   {"x" [100 3000],
                                    "m" [200 2000],
                                    "a" [300 1000],
                                    "s" [400 900]}}))))

    (is (= (set [{:rule :A,
                  :intervals {"x" [1 4000], "m" [1 4000], "a" [1 4000], "s" [3449 4000]}}
                 {:rule :lnx,
                  :intervals {"x" [1 4000], "m" [1 4000], "a" [1 4000], "s" [2771 3448]}}])
           (set (process-intervals ind-rules [{:rule :qs,
                                               :intervals
                                               {"x" [1 4000], "m" [1 4000], "a" [1 4000], "s" [2771 4000]}}]))))

    (is (= (set [{:rule :px :intervals {"x" [1 max-num]
                                        "m" [1 max-num]
                                        "a" [1 max-num]
                                        "s" [1 1350]}}
                 {:rule :qqz :intervals {"x" [1 max-num]
                                         "m" [1 max-num]
                                         "a" [1 max-num]
                                         "s" [1351 4000]}}])
           (set (process-intervals ind-rules
                                   [{:rule :in :intervals {"x" [1 max-num]
                                                           "m" [1 max-num]
                                                           "a" [1 max-num]
                                                           "s" [1 max-num]}}])))))

  (is (= 167409079868000 (answer-2 input)))


 ;
  )