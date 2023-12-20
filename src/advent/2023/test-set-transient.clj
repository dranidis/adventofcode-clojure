(ns advent.2023.test-set-transient)


(defn- bubble-max-key
  "Move a maximal element of coll according to fn k (which returns a
  number) to the front of coll."
  [k coll]
  (let [max (apply max-key k coll)]
    (cons max (remove #(identical? max %) coll))))

(defn transient-union
  "Return a set that is the union of the input sets"
  {:added "1.0"}
  ([] #{})
  ([s1] s1)
  ([s1 s2]
   (let []
     (if (< (count s1) (count s2))
       (reduce conj s2 s1)
       (reduce conj s1 s2))))
  ([s1 s2 & sets]
   (let [bubbled-sets (bubble-max-key count (conj sets s2 s1))]
     (reduce into (first bubbled-sets) (rest bubbled-sets)))))

(transient-union #{12 23 43} #{43 12 5 3})



(def ts (transient #{}))

(vec ts)


(def ats1 (transient #{1 2 3}))
(def ats2 (transient #{6 2 3}))

(ats1 5)

(reduce conj! (conj! ts 5) #{1 2 3 4})

(reduce conj! (reduce conj! (conj! ts 5) (persistent! ats1)) (persistent! ats2))

(def tts ts)


(persistent! ts)


;; (defn answer-2 [input]
;;   (apply (fn [r c v]
;;            (max v)) (map (fn [[r c dir-r dir-c]]
;;                            [r c (count (all input r c dir-r dir-c))]) (confs input))))
;; (filter (fn [[r c v]] (= v 7493))
;;         (mapv (fn [[r c dir-r dir-c]]
;;                 [r c (count (all (slurp "src/advent/2023/d16/input-a.txt") r c dir-r dir-c))]) (confs (slurp "src/advent/2023/d16/input-a.txt"))))
