(ns life.gol-ui
  (:require [clojure.string :as string]
            [life.game-of-life :as gol]))

(def default-row-count 30)
(def default-col-count 30)

(defrecord GolUIState [title
                       frame-duration
                       animation-id
                       board-settings
                       current-game])

(defn make-gol-ui-state
  ([]
   (make-gol-ui-state default-row-count default-col-count))
  ([m n]
   (->GolUIState "Hello world (of Life (of Conway))!"
                 1000
                  nil
                 {:magnification 15
                  :live-cell-color   "black"
                  :dead-cell-color   "white"
                  :cell-border-color "black"}
                 (gol/make-game-of-life m n))))

(defn is-not-running? [state]
  (nil? (:animation-id state)))

(def is-running? (complement is-not-running?))

(defn make-cell [ui-state x y color]
  (let [state @ui-state
        stroke (get-in state [:board-settings :cell-border-color])]
    [:rect {:x            x
            :y            y
            :width        1
            :height       1
            :stroke       stroke
            :stroke-width 0.01
            :rx           0.1
            :fill         color
            :onClick      #(if (is-not-running? state)
                            (let [new-gol (gol/toggle-cell (:current-game state) x y)]
                              (swap! ui-state assoc-in [:current-game] new-gol)))}]))

(defn world-view [ui-state]
  (let [state @ui-state
        {:keys [] {:keys [num-rows num-cols cells]} :current-game} state
        {:keys [] {:keys [magnification live-cell-color dead-cell-color cell-border-color]} :board-settings} state]
    [:svg {:style {:width (* num-cols magnification)
                   :height (* num-rows magnification)}
           :view-box (string/join " " [0 0 num-cols num-rows])}
     (into [:g {:name "cells"}]
           (for [x (range num-cols)
                 y (range num-rows)
                 :let [cur-val (get-in cells [y x])
                       color (if (gol/is-alive? cur-val)
                               live-cell-color
                               dead-cell-color)]]
             (make-cell ui-state x y color)))]))

(defn stop-game [ui-state]
  (let [_ (js/clearInterval (:animation-id @ui-state))]
    (swap! ui-state assoc-in [:animation-id] nil)))

(defn tick! [ui-state]
  (let [next-cells (gol/transform-cells (:current-game @ui-state))]
    (swap! ui-state assoc-in [:current-game :cells] next-cells)
    (when (every? zero? (flatten next-cells))
      (stop-game ui-state))))

(defn run-game [ui-state]
  (let [anime-id (js/setInterval (fn [] (tick! ui-state)) (:frame-duration @ui-state))]
    (swap! ui-state assoc-in [:animation-id] anime-id)))

(defn game-controls [ui-state]
  (let [state @ui-state
        is-running (is-running? state)
        {:keys [] {:keys [num-rows num-cols]} :current-game} state]
    [:span
     [:input {:type     "button"
              :value    (if is-running "Stop" "Start")
              :on-click #(if is-running
                          (stop-game ui-state)
                          (run-game ui-state))}]

     [:input {:type     "button"
              :value    "Step"
              :on-click #(if (not is-running)
                          (tick! ui-state))}]

     [:input {:type     "button"
              :value    "Clear"
              :on-click #(let [new-game (gol/make-game-of-life num-rows num-cols)]
                          (when is-running?
                            (stop-game ui-state))
                          (swap! ui-state assoc-in [:current-game] new-game))}]

     [:input {:type     "button"
              :value    "Randomize"
              :on-click #(let [new-hood (vec
                                          (for [vs (partition num-rows
                                                              (repeatedly (* num-rows num-cols)
                                                                          (partial rand-int 2)))]
                                            (vec vs)))]
                          (swap! ui-state assoc-in [:current-game :cells] new-hood))}]

     [:input {:type      "range"
              :name      "Animation Step Size"
              :value     (:frame-duration state)
              :min       100
              :max       2000
              :step      100
              :on-change (fn [e]
                           (when is-running
                             (stop-game ui-state))
                           (swap! ui-state assoc-in [:frame-duration] (.-target.value e))
                           (when is-running
                             (run-game ui-state)))}]]
    ))

(defn show-game-of-life [ui-state]
  (let [state @ui-state]
    [:div
     [:h1 (:title state)]
     [:h2 (str "Is currently:" (if (is-not-running? state) " Not") " Running")]
     [world-view ui-state]
     [:div
      [game-controls ui-state]]]))

