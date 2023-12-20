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

(defn str->2D
  "Read a string containing new-lines into a 2 dimensional vector of characters"
  [input]
  (vec (for [line (str/split-lines input)]
         (vec (for [c line]
                (parse-long (str c)))))))

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
  (map (fn [mod]
         (if (= :conj (:type mod))
           (let [inputs (->> (vals modules)
                             (map (fn [m]
                                    [(:name m) (:to m)]))
                             (filter (fn [[n to]] (to (:name mod))))
                             (map (fn [[n to]] {n 0}))
                             (first))]
             (assoc mod :state inputs))
           mod))
       (vals modules)))

(comment
  (def modules (connect-conjs (parse input)))
  (def modules (connect-conjs (parse input-2)))

  ;
  )

(defn process-signal [modules [from dest signal]]
  ;; (println "PaS" modules [dest signal])
  (let [module (get modules dest)]
    (case (:type module)
      :br
      (let [signals (reduce (fn [m send-to]
                              (conj m [(:name module) send-to signal]))
                            []
                            (:to module))]
        ;; (println "SENT" signals)
        [modules signals])

      :ff (do
            ;; (println "FF" dest "received" signal)
            (if (= signal 1)
              [modules []]
              (let [signal (if (get-in modules [dest :state])
                             0
                             1)
                    modules (update-in modules [dest :state] not)
                    signals (reduce (fn [m send-to]
                                      (conj m [(:name module) send-to signal]))
                                    []
                                    (:to module))]
                ;; (println "AFTER FF" modules signals)
                [modules signals])))
      :conj (let [new-modules (assoc-in modules [dest :state from] signal)
                  signal (mod (inc (apply * (vals (get-in modules [dest :state])))) 2) ;<- old values
                  signals (reduce (fn [m send-to]
                                    (conj m [(:name module) send-to signal]))
                                  []
                                  (:to module))]
              [new-modules signals])

      [modules []])))

(defn process-single [modules times]
  (loop [modules modules
         signals [[nil :broadcaster 0]]
         low-pulses 1
         high-pulses 0
         t times]
    ;; (pprint/pprint modules)
    ;; (println signals low-pulses high-pulses)
    (if (and (= t 1) (empty? signals))
      [(* (dec low-pulses) high-pulses) (dec low-pulses) high-pulses]

      (if (empty? signals)
        (do
          ;; (println t low-pulses high-pulses)
          (recur modules
                 [[nil :broadcaster 0]]
                 (inc low-pulses)
                 high-pulses
                 (dec t)))

        (let [[modules signals] (reduce (fn [[m s] sig]
                                          (let [[mods sigs] (process-signal m sig)]
                                            [mods (apply conj s sigs)]))
                                        [modules []]
                                        signals)
              sigs (map (fn [[f t s]] s) signals)
              high (apply + sigs)
              low (- (count sigs) high)]
          (recur modules signals (+ low-pulses low) (+ high-pulses high) t))))))

(defn -main [& _]

  (pprint/pprint (process-single (parse input) 1000))
  ;; (pprint/pprint (process-single (parse input-2) 1000))
  (pprint/pprint (process-single (parse input-2) 1000))

  ;; 4784000000 too high
  ;
  )