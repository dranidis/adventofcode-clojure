(ns advent.2024.d9.core
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]))


;; Increased stack to run this
;; Add the following to project.clj
;; :jvm-opts ["-Xss4m"]


(def part 2)
(def example? false)

(def input (if example? "2333133121414131402"
               (slurp "src/advent/2024/d9/input.txt")))

(defn- parse [input]
  (mapv parse-long (str/split input #"")))

(defn- make-file
  [id size pos]
  [:file id size pos])

(defn- make-empty
  [size pos]
  [:empty 0 size pos])

(declare block-type)

(defn- is-file?
  [block]
  (= (block-type block) :file))

(defn- block-type
  [block]
  (if (= (first block) :file)
    :file
    :empty))

(defn- block-size
  [block]
  (get block 2))

(defn- file-id
  [block]
  (get block 1))

(defn- block-pos
  [block]
  (get block 3))


;; PART 1

(for [i (range 5)]
  [99 i])

(def blocks
  (loop [parsed (parse input)
         id 0
         pos 0
         blocks []
         empties []
         toogle-file? true]
    (if (empty? parsed)
      {:b (vec blocks) :e (vec empties)}
      (let [n (first parsed)]
        (if toogle-file?
          (let [new-blocks (vec (for [i (range n)]
                                  (make-file id 1 (+ pos i))))]
            (recur (rest parsed) (inc id) (+ pos n)
                   (concat blocks new-blocks) empties (not toogle-file?)))
          (let [new-empties (vec (for [i (range n)]
                                   (make-empty 1 (+ pos i))))]
            (recur (rest parsed) id (+ pos n)
                   blocks (concat empties new-empties) (not toogle-file?))))))))

(def new-blocks-e
  (loop [empties (:e blocks)
         bs (:b blocks)
         moved []]
    (if (empty? empties)
      {:b (concat bs moved) :e empties}
      (let [first-empty (first empties)
            pos (block-pos first-empty)
            last-block (peek bs)
            bl-pos (block-pos last-block)
            changed-block (make-file (file-id last-block) 1 pos)
            new-blocks (pop bs)]
        (if (> pos bl-pos)
          {:b (concat bs moved) :e empties}
          (recur (rest empties) new-blocks (conj moved changed-block)))))))

(def answer-1 (apply + (map (fn [f] (* (block-pos f) (file-id f))) (:b new-blocks-e))))


  ;; PART 2

(def blocks-2
  (loop [parsed (parse input)
         id 0
         blocks []
         toggle-file? true]
    (if (empty? parsed)
      blocks
      (let [block-size (first parsed)
            rest-parsed (rest parsed)]
        (if toggle-file?
          (recur rest-parsed (inc id) (conj blocks (make-file id block-size nil)) (not toggle-file?))
          (recur rest-parsed id (conj blocks (make-empty block-size nil)) (not toggle-file?)))))))

(defn- fit-file-in-blocks
  [in-blocks file]
  (loop [blocks in-blocks
         new-blocks []]
    (if (empty? blocks)
      nil ;; file does not fit
      (let [block (first blocks)
            rest-blocks (rest blocks)]
        (if (is-file? block)
          (recur rest-blocks (conj new-blocks block))
          (let [file-block-size (block-size file)]
            (if (< (block-size block) file-block-size)
              (recur rest-blocks (conj new-blocks block))
              (vec (concat new-blocks
                           [file (make-empty (- (block-size block) file-block-size) nil)]
                           (vec rest-blocks))))))))))

(defn- replace-last-occurrence-with-empty-block
  [v element]
  (let [index (last (keep-indexed #(when (= %2 element) %1) v))]
    (if index
      (vec (concat (subvec v 0 index)
                   (concat [(make-empty (block-size element) nil)]
                           (subvec v (inc index)))))
      v)))

(def new-blocks-2
  (loop [files (filterv is-file? blocks-2)
         blocks blocks-2]
    (if (empty? files)
      blocks
      (let [file (peek files)
            fitted (fit-file-in-blocks blocks file)
            blocks (if fitted
                     (replace-last-occurrence-with-empty-block fitted file)
                     blocks)]
        (recur (pop files) blocks)))))

(defn- blocks-2->list
  [blocks]
  (flatten (filter seq (map (fn [block]
                              (if (is-file? block)
                                (repeat (block-size block) (file-id block))
                                (repeat (block-size block) :empty)))
                            blocks))))

(def answer-2 (apply + (map (fn [[a b]] (if (not= :empty b) (* a b) 0))
                            (map-indexed vector
                                         (vec (blocks-2->list new-blocks-2))))))

(defn- -main [& _]
  (println "Day 9, Part 1:" answer-1)
  (println "Day 9, Part 2:" answer-2))






