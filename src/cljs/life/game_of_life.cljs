(ns life.game-of-life )

(defrecord GameOfLife [num-rows num-cols cells])

(defn make-game-of-life
  ([]
   (make-game-of-life 5 5))
  ([num-rows num-cols]
    (->GameOfLife num-rows num-cols (vec (repeat num-rows (vec (repeat num-cols 0)))))))

(defn neighbors [cells m n tenant]
  (let [[x y] tenant
        x- (if (zero? x) (dec m) (dec x))
        x+ (mod (inc x) n)
        y- (if (zero? y) (dec n) (dec y))
        y+ (mod (inc y) m)]
    (map (partial get-in cells) [[y x+]
                                        [y x-]
                                        [y+ x]
                                        [y- x]
                                        [y+ x+]
                                        [y+ x-]
                                        [y- x+]
                                        [y- x-]])))

(def is-alive? (complement zero?))

(defn num-living-neighbors [s]
  (apply + s))

(defn toggle-cell [game x y]
  (let [cur-val (get-in game [:cells y x])
        new-val (if (= cur-val 0) 1 0)]
    (assoc-in game [:cells y x] new-val)))

(defn transform-cells [game]
  (let [m (:num-rows game)
        n (:num-cols game)]
    (loop [y 0
           x 0
           new-cells (:cells game)]
      (cond (= y m) new-cells
            (= x n) (recur (inc y) 0 new-cells)
            :else (let [cells (:cells game)
                        ns (neighbors cells m n [x y])
                        old-val (get-in cells [y x])
                        alive (is-alive? old-val)
                        living-neighbors (num-living-neighbors ns)
                        new-val (if alive
                                  (cond (< living-neighbors 2) 0
                                        (> living-neighbors 3) 0
                                        :else 1)
                                  (cond (= 3 living-neighbors) 1
                                        :else 0))]
                    (recur y
                           (inc x)
                           (if (= new-val old-val)
                             new-cells
                             (assoc-in new-cells [y x] new-val))))))))

