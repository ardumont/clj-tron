(ns clj-tron.core
  (:require [clojure
             [pprint :as p]
             [string :as s]]))

(def arena
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

(def arena (make-arena 10 10))

(p/pprint arena)

(defn print-arena
  [arena]
  (dosync
   (doseq [row arena]
     (let [vals (map deref row)
           chars (map {:wall \X nil \space} vals)]
       (println (apply str chars))))))

(print-arena arena)

(defn stubborn-bot
  "Stubborn bot that goes straight ahead"
  [name [i j :as pos] [di dj]]
  (dosync
   (let [r (get-in arena pos)]
     (if @r
       (println name "-> boum!")
       (do
         (ref-set r name)
         [(+ i di) (+ j dj)])))))

(defn play
  "Play the strategy for the bot bot"
  [bot]
  )
