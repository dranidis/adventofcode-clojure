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
  [id size]
  [:file id size])

(defn- make-empty
  [size]
  [:empty size])

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
  (if (is-file? block)
    (get block 2)
    (get block 1)))

(defn- file-id
  [block]
  (if (is-file? block)
    (get block 1)
    nil))

;; PART 1

(def blocks
  (vec (loop [parsed (parse input)
              id 0
              blocks []
              toogle-file? true]
         (if (empty? parsed)
           blocks
           (let [n (first parsed)
                 block (repeat n (if toogle-file? id "."))
                 newid (if toogle-file? (inc id) id)]
             (recur (rest parsed) newid (concat blocks block) (not toogle-file?)))))))

(when example? (is (= "00...111...2...333.44.5555.6666.777.888899"
                      (apply str blocks))))

(defn- remove-last
  [blocks]
  (loop [blocks blocks]
    (if (empty? blocks)
      [nil []]
      (let [b (peek blocks)]
        (if (= b ".")
          (recur (pop blocks))
          [b (pop blocks)])))))

(def new-blocks
  (loop [blocks blocks
         newblocks []]
    (if (empty? blocks)
      newblocks
      (let [b (first blocks)]
        (if (= b ".")
          (let [[last-block rest-blocks] (remove-last (vec (rest blocks)))
                new-blocks (if last-block
                             (conj newblocks last-block)
                             newblocks)]
            (recur rest-blocks new-blocks))
          (recur (vec (rest blocks)) (conj newblocks b)))))))

(when example? (is (= "0099811188827773336446555566" (apply str new-blocks))))

(def answer-1 (apply + (map (fn [[a b]] (* a b))
                            (map-indexed vector
                                         (vec (filter (fn [b] (not= b "."))
                                                      new-blocks))))))
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
          (recur rest-parsed (inc id) (conj blocks (make-file id block-size)) (not toggle-file?))
          (recur rest-parsed id (conj blocks (make-empty block-size)) (not toggle-file?)))))))

(defn- blocks-2->str
  [blocks-2]
  (apply str (map (fn [block]
                    (if (is-file? block)
                      (apply str (repeat (block-size block) (str (file-id block))))
                      (apply str (repeat (block-size block) "."))))
                  blocks-2)))

(when (and example? (= part 2))
  (is (= "00...111...2...333.44.5555.6666.777.888899"
         (blocks-2->str blocks-2))))

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
                           [file (make-empty (- (block-size block) file-block-size))]
                           (vec rest-blocks))))))))))

(defn- replace-last-occurrence-with-empty-block
  [v element]
  (let [index (last (keep-indexed #(when (= %2 element) %1) v))]
    (if index
      (vec (concat (subvec v 0 index)
                   (concat [(make-empty (block-size element))]
                           (subvec v (inc index)))))
      v)))

(when (and example? (= part 2))
  (is (= "0099.111...2...333.44.5555.6666.777.8888.." (blocks-2->str (replace-last-occurrence-with-empty-block
                                                                      (fit-file-in-blocks blocks-2 (last blocks-2))
                                                                      (last blocks-2))))))

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






