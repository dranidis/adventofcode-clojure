(ns advent.2023.d17-test
  (:require [advent.2023.d17.core :refer [is-move-valid? move possible-moves
                                          start]]
            [clojure.test :refer [deftest is]]))



(deftest part-1
  (is (not (-> start
               (move :rt)
               (move :rt)
               (move :rt)
               (is-move-valid? :rt))))

  (is (not (-> start
               (move :rt)
               (move :rt)
               (move :rt)
               (is-move-valid? :lf))))

  (is (-> start
          (move :rt)
          (move :rt)
          (is-move-valid? :rt)))

  (is (not (-> start
               (move :rt)
               (move :dn)
               (move :dn)
               (move :dn)
               (is-move-valid? :dn))))

  (is (not (-> start
               (move :rt)
               (move :dn)
               (move :lf)
               (is-move-valid? :up))))

  (is (-> start
          (move :rt)
          (move :dn)
          (move :dn)
          (is-move-valid? :dn)))

  (is (= 4 (:heat-loss (-> start (move :rt)))))
  (is (= 3 (:heat-loss (-> start (move :dn)))))

  (is (is-move-valid? start :dn))
  (is (is-move-valid? start :rt))
  (is (not (is-move-valid? start :lf)))
  (is (not (is-move-valid? start :up)))

  (is (= #{:rt :dn} (possible-moves start)))
  (is (= #{:rt :dn} (possible-moves (-> start (move :rt)))))
  (is (= #{:lf :rt :dn} (possible-moves (-> start (move :rt) (move :dn)))))

  ;
  )