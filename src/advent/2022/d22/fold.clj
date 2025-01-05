(ns advent.2022.d22.fold)

;; Attempting to create stiching alg
(def net {"1" {:L "2" :B "3"}
          "2" {:L "1"}
          "3" {:T "1" :B "4"}
          "4" {:T "3" :L "5"}
          "5" {:R "4" :B "6"}
          "6" {:T "5"}})

(defn- eq [a b]
  (and (some? a) (some? b) (= a b)))

(def upd-net (let [adjA (for [[a ac] net
                              [b bc] net
                              :when (eq (:L ac) (:T bc))]
                          [a b])
                   adjB (for [[a ac] net
                              [b bc] net
                              :when (eq (:B ac) (:R bc))]
                          [a b])
                   net (reduce (fn [n [a b]]
                                 (-> n
                                     (assoc-in [a :B] {:xL b})
                                     (assoc-in [b :R] {:xR a})))
                               net
                               adjA)
                   net (reduce (fn [n [a b]]
                                 (-> n
                                     (assoc-in [a :L] {:xR b})
                                     (assoc-in [b :T] {:xL a})))
                               net
                               adjB)]
               net))

(for [[a ac] upd-net
      [b bc] upd-net
      :when (eq (get-in ac [:B :xL]) (get-in bc [:R :xR]))]
  [a b])

(defn empty-edges [face]
  (filterv (fn [e] (nil? (get face e))) [:L :R :B :T]))

(defn empty-edges-net [net]
  (mapv (fn [x] (println x)
          [(first x) (empty-edges (second x))])
        (into [] net)))

(empty-edges-net net)