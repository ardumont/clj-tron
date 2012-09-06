(ns clj-tron.core
  (:require [clojure.pprint :as p]))

(def arena
  (vec (repeat 10 (repeat 10 nil))))

(defn make-arena
  "Build a new arena nxn"
  [n]
  (vec (repeat n (repeat n nil))))

(def arena (make-arena 10))

(p/pprint arena)
