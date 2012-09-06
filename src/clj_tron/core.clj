(ns clj-tron.core
  (:require [clojure
             [pprint :as p]
             [string :as s]]))

#_(def arena
  (vec (repeat 10 (repeat 10 nil))))

(defn make-arena
  "Build a new arena w x h"
  [w h]
  (vec (repeatedly w (fn [] (vec (repeatedly h #(ref nil)))))))

;; ou

(defn make-arena
  "Build a new arena w x h"
  [w h]
  (let [arena (->>
               (repeatedly #(ref nil))
               (partition h)
               (map vec)
               (take w)
               vec)]
    (dosync
     (doseq [i (range w)]
       (ref-set (get-in arena [i 0]) :wall)
       (ref-set (get-in arena [i (dec h)]) :wall))
     (doseq [j (range h)]
       (ref-set (get-in arena [0 j]) :wall)
       (ref-set (get-in arena [(dec w) j]) :wall)))
    arena))

#_(def arena (make-arena 10 10))

#_(p/pprint arena)

(defn print-arena
  [arena]
  (dosync
   (doseq [row arena]
     (let [vals (map deref row)
           chars (map #({:wall \X nil \space} % %) vals)]
       (println (apply str chars))))))

#_(print-arena arena)

(defn stubborn-bot-factory
  "Stubborn bot that goes straight ahead"
  [[di dj]]
  (fn [[i j]]
    [(+ i di) (+ j dj)]))

(defn stubborn-bot-factory
  "Drunk bot that goes randomly"
  [[di dj]]
  {:strategy (fn [[i j] _]
               {:pos [(+ i di) (+ j dj)]
                :state nil})
   :state nil})

(defn avoid-bot-factory
  "Generates bot that goes straight ahead and avoid obstacles!"
  )

(defn play
  "Play the strategy for the bot bot. A strategy takes one position or nil if impossible."
  [name {bot :strategy
         init-state :state} init-pos]
  (loop [{pos :pos
          bot-state :state}
         {:pos init-pos
          :state init-state}]
    (when pos
      (Thread/sleep 2000)
      (recur
       (dosync
        (let [r (get-in arena pos)]
          (if @r
            (println name "ARGGGGGHHHHH! BOUM!")
            (do
              (ref-set r name)
              (bot pos bot-state)))))))))

#_(play 1 (stubborn-bot-factory [1 0]) [3 3])

(def arena (make-arena 20 20))

(print-arena arena)

(do
  (future (play \o (stubborn-bot-factory [1 0])  [1 1]))
  (future (play \z (stubborn-bot-factory [0 -1]) [8 8])))


