(ns clj-tron.core
  (:require [clojure
             [pprint :as p]
             [string :as s]]))

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
  [arena [di dj]]
  {:strategy (fn [[i j] _]
               {:pos [(+ i di) (+ j dj)]
                :state nil})
   :state nil})

;; Behaviour to turn left at each obstacle
(def to-left
  {[0 1]  [-1 0]
   [-1 0] [0 -1]
   [0 -1] [1 0]
   [1 0]  [0 1]})

(defn avoider-bot-factory
  "Generates bot that goes straight ahead and avoid obstacles!"
  [arena [di dj :as dir]]
  {:strategy
   (fn [pos dir]
     (let [new-pos (map + pos dir)]
       (if @(get-in arena new-pos)
         (let [new-dir (to-left dir)
               new-pos (map + pos new-dir)]
           {:pos new-pos :state new-dir})
         {:pos new-pos :state dir})))
   :state dir})

(defn play
  "Play the strategy for the bot bot. A strategy takes one position or nil if impossible."
  [arena name {bot :strategy
               init-state :state} init-pos]
  (loop [{pos :pos
          bot-state :state}
         {:pos init-pos
          :state init-state}]
    (when pos
      (Thread/sleep 400)
      (recur
       (dosync
        (let [r (get-in arena pos)]
          (if @r
            (println name "ARGGGGGHHHHH! BOUM!")
            (do
              (ref-set r name)
              (bot pos bot-state)))))))))

#_(def arena (make-arena 20 20))

#_(let [arena (make-arena 20 20)]
  (future (play arena \o (avoider-bot-factory arena [1 0])  [3 3]))
  (future (play arena \z (avoider-bot-factory arena [0 -1]) [8 8]))
  (future (play arena \a (avoider-bot-factory arena [-1 0]) [2 1])))

#_(print-arena arena)

(defn display-arena
  "Display regularly the arena according to the print-fn function (which takes an arena as parameter)"
  [print-fn n]
  (fn [arena]
    (repeatedly n
                #(dosync (print-fn arena)
                         (Thread/sleep 400)) )))

(def display-arena-print (display-arena print-arena 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *size-cell 20);; size of the cell
(def *offset 29)   ;; for the border drawn in gnome (do not work under stumpwm)

(defn get-gfx "Given a width and a height, returns a frame with these dimension"
  [width height]
  (.getGraphics
   (doto (javax.swing.JFrame.)
     (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
     (.setSize width height)
     (.setVisible true))))

(defn random-arena! "Generate a random number of tron bots"
  [n]
  (do
    (let [arena (make-arena n n)]
      (future (play arena \o (avoider-bot-factory arena [1 0])  [3 3]))
      (future (play arena \z (avoider-bot-factory arena [0 -1]) [8 8]))
      (future (play arena \a (avoider-bot-factory arena [-1 0]) [2 1])))))

(defn- draw-cell!
  "Given a color and a cell's coordinate, draw the cell with the color col"
  [gfx col x y]
  (.setColor gfx col)
  (.fillRect gfx
             (* *size-cell x)
             (+ *offset (* *size-cell y))
             *size-cell *size-cell))

(defn draw-arena!
  [gfx arena]
  (dosync
   (doseq [x (count arena)
           y (count (first arena))]
     (let [v @(get-in arena [x y])
           c #({:wall java.awt.Color/BLACK
                nil   java.awt.Color/WHITE} val java.awt.Color/BLUE)]
       (draw-cell! gfx c x y)))))

(defn tron! "tron"
  ([n]
     (tron! n (random-arena! n)))
  ([n arena]
     (let [w (* *size-cell n)
           h (* *size-cell n)
           gfx (get-gfx w h)]
       (do (draw-arena! gfx arena)
           (Thread/sleep 400)))))

#_(display-arena-print arena)
