(ns ^:figwheel-always life.core
    (:require
              [reagent.core :as reagent :refer [atom]]
              [life.gol-ui :as gol]))

(enable-console-print!)

(defonce app-state (atom (gol/make-gol-ui-state)))

(defn render! []
  (reagent/render-component [gol/show-game-of-life app-state]
                              (. js/document (getElementById "app"))))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (println "reloading...")
  (render!)
  (println "done rendering!")
)

(render!)
