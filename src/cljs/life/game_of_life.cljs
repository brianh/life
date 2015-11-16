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

(defn transform-cells [{:keys [num-rows num-cols cells]}]
  (loop [y 0
         x 0
         new-cells cells]
    (cond (= y num-rows) new-cells
          (= x num-cols) (recur (inc y) 0 new-cells)
          :else (let [ns (neighbors cells num-rows num-cols [x y])
                      old-state (get-in cells [y x])
                      live-neighbor-count (num-living-neighbors ns)
                      new-state (if (is-alive? old-state)
                                (cond (< live-neighbor-count 2) 0
                                      (> live-neighbor-count 3) 0
                                      :else 1)
                                (cond (= 3 live-neighbor-count) 1
                                      :else 0))]
                  (recur y
                         (inc x)
                         (if (= new-state old-state)
                           new-cells
                           (assoc-in new-cells [y x] new-state)))))))
