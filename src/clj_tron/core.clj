(ns clj-tron.core
  (:require [clojure.pprint :as p]))

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
  (->>
   (repeatedly #(ref nil))
   (partition h)
   (map vec)
   (take w)
   vec))

(def arena (make-arena 10 10))

(p/pprint arena)

