(ns clj-tron.core
  (:require [clojure
             [pprint :as p]
             [string :as s]]
            [clj-tron.bots :as b]))

;; Some small game of tron started in the code retreat 'intro to clojure' with Christophe Grand himself main function
;; tron! wall are black when a bot dies, the cell in which it crashes become orange in the colorized version, the name
;; is the color of the bot

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
            (ref-set r :dead)
            (do
              (ref-set r name)
              (bot pos bot-state)))))))))

#_(def arena (make-arena 20 20))

#_(let [arena (make-arena 20 20)]
  (future (play arena \o (b/avoider-bot-factory arena [1 0])  [3 3]))
  (future (play arena \z (b/avoider-bot-factory arena [0 -1]) [8 8]))
  (future (play arena \a (b/avoider-bot-factory arena [-1 0]) [2 1])))

#_(print-arena arena)

(defn display-arena
  "Display regularly the arena according to the print-fn function (which takes an arena as parameter)"
  [print-fn n]
  (fn [arena]
    (repeatedly n
                #(dosync (print-fn arena)
                         (Thread/sleep 400)))))

(def display-arena-print (display-arena print-arena 10))

#_(display-arena-print arena)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *size-cell 30);; size of the cell
(def *offset 29)   ;; for the border drawn in gnome (do not work under stumpwm)

(defn get-gfx "Given a width and a height, returns a frame with these dimension"
  [w h]
  (.getGraphics
   (doto (javax.swing.JFrame.)
     (.setDefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE)
     (.setSize w h)
     (.setVisible true))))

(defn random-arena! "Creates an arena and setup a random number of tron bots"
  [n]
  (do
    (let [arena (make-arena n n)]
      (future
        (play arena
              java.awt.Color/RED
              (b/avoider-bot-factory arena [1 0])
              [3 3]))
      (future
        (play arena
              java.awt.Color/GREEN
              (b/avoider-bot-factory arena [0 -1])
              [8 8]))
      (future
        (play arena
              java.awt.Color/BLUE
              (b/avoider-bot-factory arena [-1 0])
              [2 1]))
      arena)))

(defn- draw-cell!
  "Given a color and a cell's coordinate, draw the cell with the color col"
  [^java.awt.Graphics2D gfx ^java.awt.Color col x y]
  (.setColor gfx col)
  (.fillRect gfx
             (* *size-cell x)
             (+ *offset (* *size-cell y))
             *size-cell *size-cell))

(defn draw-arena!
  [gfx arena]
  (dosync
   (doseq [x (range (count arena))
           y (range (count (first arena)))]
     (let [v @(get-in arena [x y])
           c ({:wall java.awt.Color/BLACK
               :dead java.awt.Color/ORANGE
               nil   java.awt.Color/WHITE} v v)]
       (draw-cell! gfx c x y))))
  arena)

(defn tron! "tron"
  ([n]
     (tron! n (random-arena! n) draw-arena!))
  ([n arena draw-fn!]
     (let [w (* *size-cell n)
           h (* *size-cell n)
           ^java.awt.Graphics2D gfx (get-gfx w h)]
       (loop [arena arena]
         (let [arena (draw-fn! gfx arena)]
             (Thread/sleep 500)
             (recur arena))))))

#_(display-arena-print arena)

#_ (tron! 20)
