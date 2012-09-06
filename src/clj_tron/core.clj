(ns clj-tron.core
  (:require [clojure.pprint :as p]))

(def arena
  (vec (repeat 10 (repeat 10 nil))))

(defn make-arena
  "Build a new arena w x h"
  [w h]
  (vec (repeat w (repeat h (ref nil)))))

(def arena (make-arena 10 10))

(p/pprint arena)

