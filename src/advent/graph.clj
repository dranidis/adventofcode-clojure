(ns advent.graph
  (:require
   [clojure.set :as set]))

;; Bron-Kerbosch with pivoting

(defn- bron-kerbosch

  "Find all maximal cliques in a graph using the Bron-Kerbosch algorithm with pivoting.
   A clique is a set of vertices where each pair of vertices is connected by an edge.
   
   The parameters are:
    - r: The current clique
    - p: The set of candidates
    - x: The set of excluded vertices
    - graph: The graph as an adjacency list
    - cliques: The list of maximal cliques found so far
   
   It returns the list of maximal cliques found so far.
   
   At each recursive call, 
   it finds all maximal cliques containing the vertices in r and p, but not in x,
   and adds them to the list of maximal cliques.

   It uses pivoting to reduce the number of recursive calls.

   
   The algorithm is based on the paper 'Finding All Cliques of an Undirected Graph' 
   by Bron and Kerbosch (1973)
   https://dl.acm.org/doi/10.1145/362342.362367"

  [r p x graph cliques]
  (if (and (empty? p) (empty? x))
    ;; Found a maximal clique
    (conj cliques r)
    (let [u (first (set/union p x))] ;; Choose a pivot u
      (reduce
       (fn [acc v]
         (bron-kerbosch
          (conj r v)                               ;; Add v to the current clique
          (set/intersection p (get graph v #{}))   ;; Candidates: Neighbors of v in P
          (set/intersection x (get graph v #{}))   ;; Excluded: Neighbors of v in X
          graph
          acc))
       cliques
       (set/difference p (get graph u #{}))))))   ;; Iterate over P \ N(u)

;; Find all maximal cliques
(defn maximal-cliques
  "Find all maximal cliques in a graph using the Bron-Kerbosch algorithm with pivoting.
   A clique is a set of vertices where each pair of vertices is connected by an edge"
  [graph]
  (bron-kerbosch #{} (set (keys graph)) #{} graph #{}))

;; Find the maximum clique
(defn maximum-clique
  "Find the maximum clique in a graph using the Bron-Kerbosch algorithm with pivoting.
   A clique is a set of vertices where each pair of vertices is connected by an edge"
  [graph]
  (apply max-key count (maximal-cliques graph)))

(comment
;; ;; Example graph as an adjacency list
  (def graph
    {1 #{2 3 5}
     2 #{1 3 6}
     3 #{1 2 4}
     4 #{3 5 6}
     5 #{1 4 6 7}
     6 #{2 4 5 7}
     7 #{4 5 6}})

    ;;   1
    ;;  /|\
    ;; 2_3 5__
    ;; | |/|  \
    ;; | 4_7  |
    ;; | \|  |
    ;; \_6__/  

  (maximal-cliques graph)
;;   #{#{4 3} #{7 4 6 5} #{6 2} #{1 5} #{1 3 2}}

  (maximum-clique graph)
;; #{7 4 6 5}
  )

