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

;; (def module {:name :con, :type :conj, :to #{:output}, :state nil})
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

(comment
  (def modules (parse input-2))

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
                  ;; _ (println "CONJ state " (get new-modules dest))
                  signal (mod
                          (inc (apply *
                                      (vals (get-in new-modules [dest :state])))) 2) ;<- old values? or new
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
      (* low-pulses high-pulses)

      (if (empty? signals)
        (do
          ;; (println "T" t "L" low-pulses  "H" high-pulses)
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
              low (- (count sigs) high)
              sigs-to-rx (filter (fn [[f t s]] (and (= 0 s) (= t :rx))) signals)
              _ (when (seq sigs-to-rx)
                  (println "TIME AT " t signals))]

          (recur modules signals (+ low-pulses low) (+ high-pulses high) t))))))

;; (pprint/pprint (process-single (parse input) 10))
(comment

  (get (connect-conjs (parse (slurp "src/advent/2023/d20/input.txt"))) :gf)
  ;
  )

(defn -main [& _]

  ;; (pprint/pprint (process-single (connect-conjs (parse input)) 1000))
  ;; (pprint/pprint (process-single (connect-conjs (parse input-2)) 1000))
  (pprint/pprint (process-single (connect-conjs (parse (slurp "src/advent/2023/d20/input.txt"))) 1000))
  ;
  )