(ns ogres.app.component.scene-pattern
  (:require [uix.core :refer [defui $]]))

(defui ^:private pattern-solid
  [props]
  ($ :pattern {:id (:id props) :pattern-units "userSpaceOnUse" :width 10 :height 10}
    ($ :rect {:width 10 :height 10 :stroke "none"})))

(defui ^:private pattern-lines
  [props]
  ($ :pattern {:id (:id props) :pattern-units "userSpaceOnUse" :width 20 :height 20}
    ($ :path
      {:d "M 0,20 l 20,-20 M -5,5 l 10,-10 M 15,25 l 10,-10"
       :fill "transparent"
       :stroke-width 1
       :stroke-linecap "square"})))

(defui ^:private pattern-circles
  [props]
  ($ :pattern {:id (:id props) :pattern-units "userSpaceOnUse" :width 12 :height 12}
    ($ :circle {:cx 7 :cy 7 :r 3 :fill "transparent"})))

(defui ^:private pattern-crosses
  [props]
  ($ :pattern {:id (:id props) :pattern-units "userSpaceOnUse" :width 12 :height 12}
    ($ :path
      {:d "M 2.5,2.5l5,5M2.5,7.5l5,-5"
       :fill "transparent"
       :stroke-width 1
       :shape-rendering "auto"
       :stroke-linecap "square"})))

(defui ^:private pattern-caps
  [props]
  ($ :pattern {:id (:id props) :pattern-units "userSpaceOnUse" :width 12 :height 12}
    ($ :path
      {:d "M 2.5,7.5l2.5,-5l2.5,5"
       :fill "transparent"
       :stroke-width 1
       :stroke-linecap "square"})))

(defui ^:private pattern-waves
  [props]
  ($ :pattern {:id (:id props) :pattern-units "userSpaceOnUse" :width 10 :height 10}
    ($ :path
      {:d "M 0 5 c 1.25 -2.5 , 3.75 -2.5 , 5 0 c 1.25 2.5 , 3.75 2.5 , 5 0 M -5 5 c 1.25 2.5 , 3.75 2.5 , 5 0 M 10 5 c 1.25 -2.5 , 3.75 -2.5 , 5 0"
       :fill "transparent"
       :stroke-width 2
       :stroke-linecap "square"})))

(defui ^:private pattern-empty
  [props]
  ($ :pattern {:id (:id props) :pattern-units "userSpaceOnUse" :width 10 :height 10}))

(defui pattern [{:keys [name] :as props}]
  (case name
    :caps    ($ pattern-caps props)
    :circles ($ pattern-circles props)
    :crosses ($ pattern-crosses props)
    :lines   ($ pattern-lines props)
    :solid   ($ pattern-solid props)
    :waves   ($ pattern-waves props)
    :empty   ($ pattern-empty props)))
