(ns advent.2023.d17.priority-queue
  (:import [clojure.lang PersistentVector]
           [java.util LinkedList PriorityQueue]))

(defn make-priority-queue!
  "Create a mutable PriorityQueue. 
   If no comparator is provided, the default
   value comparison is used for determining priorities."
  ([] (new PriorityQueue))
  ([comparator] (new PriorityQueue comparator)))

(defn add-with-priority!
  "Add value to the priority queue."
  ([queue] queue)
  ([queue v]
   (.add queue v)
   queue)
  ([queue v & vs]
   (if vs
     (do
       (add-with-priority! queue v)
       (recur queue (first vs) (next vs)))
     (do (add-with-priority! queue v)
         queue))))

(defn extract-min!
  "remove and return min value"
  [queue]
  (. queue poll))

(defn change-value!
  "Removes the old value and adds the new value."
  [queue old-v v]
  (.remove queue old-v)
  (.add queue v))

;; (defn queue->list [queue]
;;   (map (fn [[x1 x2 _]] [x1 x2]) (.toArray queue)))

(comment

  (def q (make-priority-queue!
          (fn [^PersistentVector xs, ^PersistentVector ys]
            (< (peek xs) (peek ys)))))
  (add-with-priority! q [4 1 1])
  (add-with-priority! q [2 2 2])
  (add-with-priority! q [1 3 3])
  (add-with-priority! q [5 4 4] [1 2 1])

  (extract-min! q)

  ;;;;;;

  (def q (make-priority-queue!))
  (add-with-priority! q 2)
  (add-with-priority! q 1)
  (add-with-priority! q 5)
  (add-with-priority! q 3)
  (extract-min! q)

  ;
  )

