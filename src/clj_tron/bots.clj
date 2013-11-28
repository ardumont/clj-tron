(ns clj-tron.bots)

(defn stubborn-bot-factory "Drunk bot that goes randomly"
  [arena [di dj]]
  {:strategy (fn [[i j] _]
               {:pos [(+ i di) (+ j dj)]
                :state nil})
   :state nil})

(def to-right "Behaviour to turn right at each obstacle"
  {[0 1]  [-1 0]
   [-1 0] [0 -1]
   [0 -1] [1 0]
   [1 0]  [0 1]})

(defn- new-pos-dir "Compute the new position and the new direction and return a vector [new-pos new-dir]"
  [pos dir new-dir-fn]
  (let [new-dir (new-dir-fn dir)]
    {:pos (map + pos new-dir)
     :state new-dir}))

(defn avoider-right-bot-factory "Generates bot that goes straight ahead and avoid obstacles by turning right!"
  [arena [di dj :as dir]]
  {:strategy
   (fn [pos dir]
     (let [new-pos (map + pos dir)]
       (if @(get-in arena new-pos)
         (new-pos-dir pos dir to-right)
         {:pos new-pos :state dir})))
   :state dir})

(def to-left "Behaviour to turn left at each obstacle"
  {[0 1]  [1 0]
   [1 0]  [0 -1]
   [0 -1] [-1 0]
   [-1 0] [0 1]})

(defn avoider-bot-factory "Generates bot that goes straight ahead and avoid obstacles by trying to turn left or right!"
  [arena [di dj :as dir]]
  {:strategy
   (fn [posi dir]
     (let [new-pos (map + posi dir)]
       (if @(get-in arena new-pos)
         (if (zero? (mod (rand-int 1000) 2));; even -> left, odd -> right
           (let [{:keys [pos] :as new-res-left} (new-pos-dir posi dir to-left)]
             (if @(get-in arena pos)
               (new-pos-dir pos dir to-right))
             new-res-left)
           (let [{:keys [pos] :as new-res-right} (new-pos-dir posi dir to-right)]
             (if @(get-in arena pos)
               (new-pos-dir pos dir to-left))
             new-res-right))
         {:pos new-pos :state dir})))
   :state dir})
