(ns advent.2023.d16-test
  (:require [advent.2023.d16.core :refer [all answer-1 answer-2
                                          flip-directions]]
            [clojure.test :refer [deftest is]]))

(def input (slurp "src/advent/2023/d16/example.txt"))

(deftest  p1
  (is (= [1 0] (flip-directions 0 1 "\\")))
  (is (= [-1 0] (flip-directions 0 -1 "\\")))
  (is (= [0 1] (flip-directions 1 0 "\\")))
  (is (= [0 -1] (flip-directions -1 0 "\\")))

  (is (= [-1 0] (flip-directions 0 1 "/")))
  (is (= [1 0] (flip-directions 0 -1 "/")))
  (is (= [0 -1] (flip-directions 1 0 "/")))
  (is (= [0 1] (flip-directions -1 0 "/")))


  (is (= 9
         (count (all  ".\\.
//.
\\./"  0 0 0 1))))


  (is (= 3 (count (all ".\\\n..\\n" 0 0 0 1))))


  (is (= 5 (count (all  ".\\.
.-.
..." 0 0 0 1))))

  (is (= 5 (count (all  ".\\.
\\-.
..." 0 0 0 1))))

  (is (= 9 (count (all  ".\\.
//.
\\./" 0 0 0 1))))




  (is (= 46 (answer-1 input)))
  (is (= 51 (answer-2 input)))
  ;
  )