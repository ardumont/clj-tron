(ns clj-tron.bots)

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
