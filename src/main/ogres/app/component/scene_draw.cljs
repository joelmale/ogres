(ns ogres.app.component.scene-draw
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.const :refer [grid-size half-size]] ;; Assuming world-line-thickness is also in const
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.matrix :as matrix]
            [ogres.app.util :as util]
            [ogres.app.vec :as vec :refer [Segment Vec2]]
            [uix.core :as uix :refer [defui $]]
            ["@dnd-kit/core"
             :refer  [DndContext useDraggable useDndMonitor]
             :rename {DndContext    dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable}]))

(defn instance [x]
  (cond (instance? Vec2 x) Vec2
        (instance? Segment x) Segment))

(defmulti align-identity instance)
(defmethod align-identity Vec2 [a] a)
(defmethod align-identity Segment [s] s)

(defmulti align-grid instance)
(defmethod align-grid Vec2 [a]
  (vec/rnd a grid-size))
(defmethod align-grid Segment [s]
  (Segment. (align-grid (.-a s)) (align-grid (.-b s))))

(defmulti align-grid-half instance)
(defmethod align-grid-half Vec2 [a] (vec/rnd a half-size))
(defmethod align-grid-half Segment [s]
  (Segment. (align-grid-half (.-a s)) (align-grid-half (.-b s))))

(defmulti align-line instance)
(defmethod align-line Vec2 [a] (align-grid-half a))
(defmethod align-line Segment [s]
  (let [src (align-grid-half (.-a s))
        dir (vec/sub (.-b s) src)
        len (vec/dist vec/zero dir)]
    (if (= len 0)
      (Segment. src src)
      (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
        (Segment. src dst)))))

(defmulti align-cone instance)
(defmethod align-cone Vec2 [a] (align-grid a))
(defmethod align-cone Segment [s]
  (let [src (align-grid (.-a s))
        dir (vec/sub (.-b s) src)
        len (vec/dist vec/zero dir)]
    (if (= len 0)
      (Segment. src src)
      (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
        (Segment. src dst)))))

(def ^:private points->poly
  (completing into (fn [xs] (join " " xs))))

(defn ^:private px->ft [len]
  (let [ft (* (/ len grid-size) 5)
        rd (js/Math.round ft)]
    (if (< (abs (- ft rd)) 0.001) rd
        (.toFixed ft 1))))

(defui ^:private text
  [{:keys [attrs children]}]
  ($ :text.scene-text attrs children))

(defui ^:private anchor [props]
  ($ :g {:transform (:transform props)}
    ($ :circle.scene-draw-anchor {:r 4})
    ($ :circle.scene-draw-anchor-ring {:r 6})))

(defui ^:private draw-segment-drag [props]
  (let [{:keys [children on-release use-cursor]} props
        [segment set-segment] (uix/use-state nil)
        [cursor   set-cursor] (uix/use-state nil)
        options (use-draggable #js {"id" "drawable"})
        on-down (.. options -listeners -onPointerDown)
        on-stop
        (fn []
          (when (not (nil? segment))
            (set-segment nil)
            (set-cursor nil)
            (on-release segment)))
        on-drag
        (uix/use-callback
         (fn [data]
           (let [dx (.-x (.-delta data))
                 dy (.-y (.-delta data))
                 mx (.-clientX (.-activatorEvent data))
                 my (.-clientY (.-activatorEvent data))]
             (set-segment
              (fn [s]
                (if (some? s)
                  (Segment. (.-a s) (vec/add (.-a s) (Vec2. dx dy)))
                  (Segment. (Vec2. mx my) (Vec2. mx my))))))) [])
        on-move
        (uix/use-callback
         (fn [event]
           (set-cursor (Vec2. (.-clientX event) (.-clientY event)))) [])]
    (use-dnd-monitor
     #js {"onDragMove" on-drag
          "onDragEnd"  on-stop})
    ($ :<>
      ($ :rect.scene-draw-surface
        {:ref (.-setNodeRef options)
         :on-pointer-down on-down
         :on-pointer-move (if (and use-cursor (nil? segment)) on-move)})
      (children segment cursor))))

(def ^:private query
  [[:user/bounds :default vec/zero-segment]
   {:user/camera
    [[:camera/scale :default 1]
     [:camera/point :default vec/zero]
     {:camera/scene
      [[:scene/grid-align :default false]
       [:scene/grid-origin :default vec/zero]
       [:scene/grid-size :default grid-size]
       [:scene/show-object-outlines :default true]]}]}])

(defui ^:private draw-segment [props]
  (let [{:keys [children on-release tile-path align-fn]
         :or {on-release :default align-fn align-identity}} props
        result (hooks/use-query query)
        {bounds :user/bounds
         {camera-world-pos :camera/point
          camera-scale :camera/scale
          {grid-paths :scene/show-object-outlines
           grid-align :scene/grid-align}
          :camera/scene}
         :user/camera} result
        align-tool-fn (if grid-align align-fn align-identity)

        screen_to_world_matrix (matrix/scale (matrix/translate matrix/identity camera-world-pos) (/ camera-scale))
        world_to_screen_matrix (matrix/inverse screen_to_world_matrix)

        transform_segment_fn (fn [segment matrix]
                               (when segment
                                 (Segment. (vec/transform (.-a segment) matrix)
                                           (vec/transform (.-b segment) matrix))))
        transform_point_fn (fn [point matrix]
                             (when point (vec/transform point matrix)))]

    ($ draw-segment-drag
      {:use-cursor (contains? props :align-fn)
       :on-release
       (fn [raw-screen-segment-from-drag]
         (let [world-segment (transform_segment_fn raw-screen-segment-from-drag screen_to_world_matrix)]
           (on-release (align-tool-fn world-segment))))}

      (fn [active-dragging-screen-segment cursor-screen-pos]
        (cond
          (some? active-dragging-screen-segment)
          (let [current-world-segment (transform_segment_fn active-dragging-screen-segment screen_to_world_matrix)
                aligned-world-segment (align-tool-fn current-world-segment)
                aligned-screen-segment (transform_segment_fn aligned-world-segment world_to_screen_matrix)]
            ($ :<>
              (if (and (fn? tile-path) grid-paths (some? aligned-world-segment))
                (let [world-path-elements (tile-path aligned-world-segment)]
                  ($ :polygon.scene-draw-tile-path
                    {:points (transduce (map (comp seq #(transform_point_fn % world_to_screen_matrix)))
                                        points->poly [] world-path-elements)})))
              (when (some? aligned-world-segment)
                (children aligned-world-segment
                          aligned-screen-segment
                          world_to_screen_matrix))))

          (and grid-align (some? cursor-screen-pos))
          (let [world-cursor-pos (transform_point_fn cursor-screen-pos screen_to_world_matrix)
                aligned-world-cursor-pos (align-tool-fn world-cursor-pos)
                screen-anchor-pos (transform_point_fn aligned-world-cursor-pos world_to_screen_matrix)]
            ($ anchor {:transform screen-anchor-pos})))))))

(defui ^:private polygon
  [{:keys [on-create]}]
  (let [result (hooks/use-query query)
        {bounds :user/bounds
         {point :camera/point
          scale :camera/scale
          {align? :scene/grid-align} :camera/scene} :user/camera} result
        [points set-points] (uix/use-state [])
        [cursor set-cursor] (uix/use-state nil)
        closing? (and (seq points) (some? cursor) (< (vec/dist (first points) cursor) 32))

        screen_to_world_matrix (matrix/scale (matrix/translate matrix/identity point) (/ scale))
        world_to_screen_matrix (matrix/inverse screen_to_world_matrix)]
    ($ :<>
      ($ :rect.scene-draw-surface
        {:on-pointer-down
         (fn [event]
           (.stopPropagation event))
         :on-pointer-move
         (fn [event]
           (let [screen-point (Vec2. (.-clientX event) (.-clientY event))
                 world-point (vec/transform screen-point screen_to_world_matrix)]
             (set-cursor (vec/rnd world-point (if align? half-size 1)))))
         :on-click
         (fn [event]
           (if (not closing?)
             (set-points (conj points cursor)) ; cursor is already world-space aligned
             (let [world-points-finalized (if closing? (conj points cursor) points) ; ensure last point added if closing
                   xs (geom/reorient (mapcat seq world-points-finalized))
                   xf (comp (partition-all 2) (map (fn [[x y]] (Vec2. x y))))]
               (set-points [])
               (set-cursor nil)
               (on-create event (into [] xf xs)))))})
      (if (and align? (not closing?) (some? cursor)) ; cursor is world-space aligned
        ($ anchor {:transform (vec/transform cursor world_to_screen_matrix)}))
      (if (seq points) ; points are world-space
        ($ :circle.scene-draw-point-ring
          {:transform (vec/transform (first points) world_to_screen_matrix) :r 6}))
      (for [point points :let [screen-point (vec/transform point world_to_screen_matrix)]] ; points are world-space
        ($ :circle.scene-draw-point
          {:key screen-point :transform screen-point :r 4}))
      (if (and (seq points) (some? cursor)) ; points & cursor are world-space
        ($ :polygon.scene-draw-shape
          {:points
           (transduce
            (map (comp seq #(vec/transform % world_to_screen_matrix)))
            points->poly []
            (if (not closing?) (conj points cursor) points))})))))

(defui ^:private draw-select []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:on-release (fn [world-segment] (dispatch :selection/from-rect world-segment))}
      (fn [world-segment screen-segment _] ; Third arg is world_to_screen_matrix, not used here
        (let [a (.-a screen-segment) b (.-b screen-segment)]
          ($ hooks/use-portal {:name :multiselect}
            ($ :path.scene-draw-shape
              {:d (join " " [\M (.-x a) (.-y a) \H (.-x b) \V (.-y b) \H (.-x a) \Z])})))))))

(defui ^:private draw-ruler []
  ($ draw-segment
    {:align-fn align-grid-half}
    (fn [world-segment screen-segment _] ; Third arg is world_to_screen_matrix, not used here
      (let [a (.-a screen-segment) b (.-b screen-segment)]
        ($ :<>
          ($ :line.scene-draw-shape
            {:x1 (.-x a) :y1 (.-y a) :x2 (.-x b) :y2 (.-y b)})
          ($ anchor {:transform a})
          ($ anchor {:transform b})
          ($ text {:attrs {:x (- (.-x b) 48) :y (- (.-y b) 8)}}
            (str (px->ft (vec/dist-cheb world-segment)) "ft.")))))))

(defui ^:private draw-circle []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release (fn [world-segment] (dispatch :shape/create :circle (seq world-segment)))
       :tile-path (fn [world-segment]
                    (let [r (vec/dist-cheb world-segment)]
                      (geom/tile-path-circle (.-a world-segment) r)))}
      (fn [world-segment screen-segment _] ; Third arg is world_to_screen_matrix, not used here
        (let [src-screen (.-a screen-segment)
              radius-world (vec/dist-cheb world-segment)]
          ($ :<>
            ($ :circle.scene-draw-shape {:transform src-screen :r (vec/dist-cheb screen-segment)})
            ($ text {:attrs {:x (.-x src-screen) :y (.-y src-screen) :fill "white"}}
              (str (px->ft radius-world) "ft. radius"))))))))

(defui ^:private draw-rect []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release (fn [world-segment] (dispatch :shape/create :rect (seq world-segment)))}
      (fn [world-segment screen-segment _] ; Third arg is world_to_screen_matrix, not used here
        (let [world-a (.-a world-segment) world-b (.-b world-segment)
              screen-a (.-a screen-segment) screen-b (.-b screen-segment)]
          ($ :<>
            ($ :path.scene-draw-shape
              {:d (join " " [\M (.-x screen-a) (.-y screen-a) \H (.-x screen-b) \V (.-y screen-b) \H (.-x screen-a) \Z])})
            ($ text {:attrs {:x (+ (.-x screen-a) 8) :y (- (.-y screen-a) 8) :fill "white"}}
              (let [v-world (vec/abs (vec/sub world-a world-b))]
                (str (px->ft (.-x v-world)) "ft. x " (px->ft (.-y v-world)) "ft.")))))))))

(defui ^:private draw-line []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-line
       :on-release (fn [world-segment] (dispatch :shape/create :line (seq world-segment)))
       :tile-path (fn [world-segment] (geom/tile-path-line (geom/line-points world-segment)))}
      (fn [world-segment screen-segment world_to_screen_matrix]
        ($ :<>
          (let [world-polygon-vertices (geom/line-points world-segment)
                screen-polygon-vertices (map #(vec/transform % world_to_screen_matrix) world-polygon-vertices)]
            ($ :polygon.scene-draw-shape
              {:points (join " " (mapcat seq screen-polygon-vertices))}))
          ($ text {:attrs {:x (.-x (.-a screen-segment)) :y (.-y (.-a screen-segment))}}
            (str (px->ft (vec/dist world-segment)) "ft.")))))))

(defui ^:private draw-cone []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-cone
       :on-release (fn [world-segment] (dispatch :shape/create :cone (seq world-segment)))
       :tile-path (fn [world-segment] (geom/tile-path-cone (geom/cone-points world-segment)))}
      (fn [world-segment screen-segment world_to_screen_matrix]
        ($ :<>
          (let [world-cone-shape-points (geom/cone-points world-segment)
                screen-cone-shape-points (map #(vec/transform % world_to_screen_matrix) world-cone-shape-points)]
            ($ :polygon.scene-draw-shape
              {:points (join " " (mapcat seq screen-cone-shape-points))}))
          ($ text {:attrs {:x (+ (.-x (.-b screen-segment)) 16) :y (+ (.-y (.-b screen-segment)) 16)}}
            (str (px->ft (vec/dist world-segment)) "ft.")))))))

(defui ^:private draw-poly []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon {:on-create (fn [_ points] (dispatch :shape/create :poly points))})))

(defui ^:private draw-mask []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon {:on-create (fn [_ points] (dispatch :mask/create (mapcat seq points)))})))

(defui ^:private draw-grid []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query) ; Added result for matrix calculation
        {bounds :user/bounds
         {shift :camera/point
          camera-scale   :camera/scale ; Renamed from scale to camera-scale for clarity
          {prev-size :scene/grid-size
           prev-origin :scene/grid-origin}
          :camera/scene} :user/camera} result
        [origin set-origin] (uix/use-state nil) ; origin is screen-space click point
        [size     set-size] (uix/use-state prev-size)

        base-world-pos (.-a bounds) ; Assuming this is a relevant world origin for grid alignment display

        ;; Matrices for transforming grid preview if origin is screen-space
        ;; This section might need adjustment based on how 'origin' (screen-space) relates to grid's world position
        screen_to_world_matrix (matrix/scale (matrix/translate matrix/identity shift) (/ camera-scale))
        world_to_screen_matrix (matrix/inverse screen_to_world_matrix)

        on-shift (fn [offset-screen] (fn [] (set-origin (fn [current-screen-origin] (vec/add current-screen-origin offset-screen)))))]
    ($ :g.grid-align
      ($ :rect.scene-draw-surface
        {:on-click
         (fn [event]
           (set-origin (Vec2. (.-clientX event) (.-clientY event))))}) ; Sets origin to screen click point
      (if (some? origin) ; origin is a screen point
        (let [rows 7
              display-grid-cell-size (* size camera-scale (/ grid-size prev-size)) ; Visual size of grid cell on screen
              wide (* display-grid-cell-size (inc rows))
              path (for [step (range (- rows) (inc rows))]
                     (str "M " (* step display-grid-cell-size) " " (- wide)      " " \V " " wide " "
                          "M " (- wide)      " " (* step display-grid-cell-size) " " \H " " wide " "))
              ;; Transform the screen origin to where the grid (0,0) should appear based on this screen click
              ;; This transform is for the visual grid rendering group
              grid-group-transform (let [world-click-equivalent (vec/transform origin screen_to_world_matrix)
                                         world-grid-zero (vec/sub world-click-equivalent base-world-pos) ; Or some other logic
                                         screen-grid-zero (vec/transform world-grid-zero world_to_screen_matrix)]
                                     (vec/sub origin base-world-pos))] ; Simplified: this positions grid (0,0) at screen 'origin' relative to 'base-world-pos's screen pos
          ($ :g {:transform (vec/sub origin (vec/transform base-world-pos world_to_screen_matrix))} ; Position grid group
            ($ :path.grid-align-path {:d (join path)})
            ($ :circle.grid-align-center {:r 6})
            ($ :foreignObject.grid-align-form
              {:x -128 :y -128 :width 256 :height 256}
              ($ :form
                {:on-submit
                 (fn [event]
                   (.preventDefault event)
                   (let [world-origin-final (-> (vec/transform origin screen_to_world_matrix) ; Clicked screen point to world
                                                (vec/add shift) ; Adjust by camera's current world position
                                                (vec/add (or prev-origin vec/zero)) ; Add previous grid offset in world
                                                (vec/mul (/ prev-size size)) ; Scale factor for new size
                                                (vec/abs)
                                                (vec/mod grid-size)
                                                (vec/rnd 0.25))]
                     (dispatch :scene/apply-grid-options world-origin-final size)))}
                ($ :fieldset.grid-align-origin
                  ($ :button {:type "button" :data-name "up"    :on-click (on-shift (Vec2. 0 -1))} ($ icon {:name "arrow-up-short" :size 20}))
                  ($ :button {:type "button" :data-name "right" :on-click (on-shift (Vec2. 1 0))}  ($ icon {:name "arrow-right-short" :size 20}))
                  ($ :button {:type "button" :data-name "down"  :on-click (on-shift (Vec2. 0 1))}  ($ icon {:name "arrow-down-short" :size 20}))
                  ($ :button {:type "button" :data-name "left"  :on-click (on-shift (Vec2. -1 0))} ($ icon {:name "arrow-left-short" :size 20}))
                  (if (not= prev-origin vec/zero)
                    ($ :button {:type "button" :data-name "clear" :data-tooltip "Reset" :on-click (fn [] (dispatch :scene/reset-grid-origin))}
                      ($ icon {:name "x-circle-fill" :size 16}))))
                ($ :fieldset.grid-align-size
                  ($ :button {:type "button" :data-name "dec" :on-click (fn [] (set-size dec))} ($ icon {:name "dash" :size 20}))
                  ($ :button {:type "button" :data-name "inc" :on-click (fn [] (set-size inc))} ($ icon {:name "plus" :size 20}))
                  ($ :input.text.text-ghost {:type "number" :value size :style {:color "white"}
                                             :on-change (fn [event] (let [n (js/Number (.. event -target -value))] (if (number? n) (set-size n))))})
                  ($ :button {:type "submit" :data-name "submit"} ($ icon {:name "check"})))))))))))

(defui ^:private draw-note []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query) ; For matrix
        {{camera-world-pos :camera/point camera-scale :camera/scale} :user/camera} result
        screen_to_world_matrix (matrix/scale (matrix/translate matrix/identity camera-world-pos) (/ camera-scale))]
    ($ :rect.scene-draw-surface
      {:on-click
       (fn [event]
         (let [screen-point (Vec2. (.-clientX event) (.-clientY event))
               world-point (vec/transform screen-point screen_to_world_matrix)]
           (dispatch :note/create world-point)))})))

(defui draw [{:keys [mode] :as props}]
  ($ dnd-context
    (case mode
      :circle ($ draw-circle props)
      :cone   ($ draw-cone props)
      :grid   ($ draw-grid props)
      :line   ($ draw-line props)
      :mask   ($ draw-mask props)
      :note   ($ draw-note props)
      :poly   ($ draw-poly props)
      :rect   ($ draw-rect props)
      :ruler  ($ draw-ruler props)
      :select ($ draw-select props))))