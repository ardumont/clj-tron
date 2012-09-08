(ns clj-tron.rand)

(defn- rand-int-not-zero
  "Compute a rand-int inside the set ]0;n["
  [n]
  {:pre (< 1 n)}
  (let [i (rand-int n)]
    (if (not= 0 i)
      i
      (recur n))))

(defn rand-pos
  "Compute a random position [x y] with in ]0;n["
  [n]
  [(rand-int-not-zero n) (rand-int-not-zero n)])

(comment
  (juxt  rand-int rand-int)
  (let [c (rand-int 2)                  ; choice for the precedence of x over y if 0
        x (rand-int n)
        y (rand-int n)]
    (if (zero? c)
      [x 0]
      [0 y])))

(defn- rand-int-neg
  "Compute a number in [-1;0[ intersect ]0;1]"
  []
  (let [r (rand-int 2)]
    (if (zero? r) -1 r)))

;; Compute a random direction (for example, [0 1], [-1 0] etc...
(defn rand-direction
  []
  (let [c (rand-int 2);; determine if we choose x or y as the direction
        r (rand-int-neg)];; the value for such
    (if (zero? c) [r 0] [0 r])))
