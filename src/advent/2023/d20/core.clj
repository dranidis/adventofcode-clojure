(ns advent.2023.d20.core
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def input "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(def input-2 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

(defn parse [input]
  (reduce (fn [acc [name type to]]
            (let [module {:name (keyword name) :type type :to (set to)}
                  module (case type
                           :ff (assoc module :state false)
                           :conj (assoc module :state nil)
                           module)]
              (assoc acc (keyword name) module)))
          {}
          (for [line (str/split-lines input)]
            (let [[from to] (str/split line #" -> ")
                  to (map keyword (str/split to #", "))
                  fc (first from)
                  name (if (= fc \b) from (apply str (rest from)))
                  type (case fc \% :ff \& :conj :br)]
              [name type to]))))

(defn connect-conjs [modules]
  (reduce (fn [acc m] (assoc acc (:name m) m))
          {}
          (map (fn [module]
                 (if (= :conj (:type module))
                   (let [inputs (zipmap (->> (vals modules)
                                             (map (fn [m]
                                                    [(:name m) (:to m)]))
                                             (filter (fn [[n to]] (to (:name module))))
                                             (map (fn [[n to]] n)))
                                        (repeat 0))]
                     (assoc module :state inputs))
                   module))
               (vals modules))))

(defn process-signal [modules [from dest signal]]
  (let [module (get modules dest)]
    (case (:type module)
      :br
      (let [signals (reduce (fn [m send-to]
                              (conj m [(:name module) send-to signal]))
                            []
                            (:to module))]
        [modules signals])

      :ff (if (= signal 1)
            [modules []]
            (let [signal (if (get-in modules [dest :state])
                           0
                           1)
                  modules (update-in modules [dest :state] not)
                  signals (reduce (fn [m send-to]
                                    (conj m [(:name module) send-to signal]))
                                  []
                                  (:to module))]
              [modules signals]))
      :conj (let [new-modules (assoc-in modules [dest :state from] signal)
                  signal (mod
                          (inc (apply *
                                      (vals (get-in new-modules [dest :state])))) 2)
                  signals (reduce (fn [m send-to]
                                    (conj m [(:name module) send-to signal]))
                                  []
                                  (:to module))]
              [new-modules signals])

      [modules []])))

(defn answer [modules times part]
  (let [D {:pg [] :sp [] :qs [] :sv []}]
    (loop [modules modules
           signals [[nil :broadcaster 0]]
           low-pulses 1
           high-pulses 0
           t 1
           D-h D
           tt 0]
      (if (and (= 1 part) (= t times) (empty? signals))
        (* low-pulses high-pulses)
        (if (and (= 2 part)
                 (every? (fn [c] (> c 2))
                         (map (fn [key] (count (get D-h key)))
                              (keys D))))
          (apply * (for [key (keys D-h)]
                     (first (map (fn [[l h]] (- h l)) (partition 2 1 (D-h key))))))

          (if (empty? signals)
            (recur modules
                   [[nil :broadcaster 0]]
                   (inc low-pulses)
                   high-pulses
                   (inc t)
                   D-h
                   (inc tt))

            (let [[modules signals] (reduce (fn [[m s] sig]
                                              (let [[mods sigs] (process-signal m sig)]
                                                [mods (apply conj s sigs)]))
                                            [modules []]
                                            signals)
                  sigs (map (fn [[f t s]] s) signals)
                  high (apply + sigs)
                  low (- (count sigs) high)

                  sigs-rx-h (filter (fn [[f to s]]
                                      (and (= to :gf) (= s 1)))
                                    signals)
                  D-h (reduce (fn [D [f to s]]
                                (update D f conj t))
                              D-h
                              sigs-rx-h)]
              (recur modules signals (+ low-pulses low) (+ high-pulses high) t D-h (inc tt)))))))))

(defn -main [& _]

  (pprint/pprint (answer
                  (connect-conjs
                   (parse (slurp "src/advent/2023/d20/input.txt"))) 1000 1))

  (pprint/pprint (answer
                  (connect-conjs
                   (parse (slurp "src/advent/2023/d20/input.txt"))) 1000 2))
  ;
  )


