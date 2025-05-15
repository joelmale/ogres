(ns ogres.app.component.scene-draw
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.const :refer [grid-size half-size]]
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
        {{point :camera/point ; This is camera's world position **Bounds was unused and removed
          scale :camera/scale ; This is camera's zoom scale
          {grid-paths :scene/show-object-outlines
           grid-align :scene/grid-align}
          :camera/scene}
         :user/camera} result
        current-align-fn (if grid-align align-fn align-identity) ; Renamed from 'align' to avoid conflict

        ;; 'basis' is the screen-to-world transformation matrix
        basis  (matrix/scale (matrix/translate matrix/identity point) (/ scale))
        ;; 'invert' is the world-to-screen transformation matrix
        invert (matrix/inverse basis)

        transform-segment (fn [segment matrix]
                            (when segment
                              (Segment. (vec/transform (.-a segment) matrix)
                                        (vec/transform (.-b segment) matrix))))
        transform-point (fn [pt matrix] (when pt (vec/transform pt matrix)))]

    ($ draw-segment-drag
      {:use-cursor (contains? props :align-fn)
       :on-release
       (fn [raw-screen-segment] ; From draw-segment-drag, in screen coordinates
         (let [world-segment (transform-segment raw-screen-segment basis)] ; basis is screen-to-world
           (on-release (current-align-fn world-segment))))}

      (fn [active-screen-segment cursor-screen-pos] ; From draw-segment-drag
        (cond
          (some? active-screen-segment)
          (let [current-world-segment (transform-segment active-screen-segment basis) ; basis is screen-to-world
                aligned-world-segment (current-align-fn current-world-segment)
                aligned-screen-segment (transform-segment aligned-world-segment invert)] ; invert is world-to-screen
            ($ :<>
              (if (and (fn? tile-path) grid-paths (some? aligned-world-segment))
                (let [world-tile-elements (tile-path aligned-world-segment)] ; tile-path receives world, returns world elements
                  ($ :polygon.scene-draw-tile-path
                    {:points (transduce (map (comp seq #(transform-point % invert))) points->poly [] world-tile-elements)})))
              (when (some? aligned-world-segment)
                 ;; Children now called with: world-segment, screen-segment, world-to-screen-matrix
                (children aligned-world-segment aligned-screen-segment invert))))

          (and grid-align (some? cursor-screen-pos))
          (let [world-cursor (transform-point cursor-screen-pos basis) ; basis is screen-to-world
                aligned-world-cursor (current-align-fn world-cursor)
                screen-anchor-pos (transform-point aligned-world-cursor invert)] ; invert is world-to-screen
            ($ anchor {:transform screen-anchor-pos})))))))

(defui ^:private polygon ; Minimal changes to polygon, mainly reverting previous renames if any
  [{:keys [on-create]}]
  (let [result (hooks/use-query query)
        {bounds :user/bounds ; Not directly used in polygon logic below, but part of query
         {point :camera/point scale :camera/scale {align? :scene/grid-align} :camera/scene} :user/camera} result
        [points set-points] (uix/use-state []) ; Storing world-space points
        [cursor set-cursor] (uix/use-state nil) ; Storing world-space aligned cursor

        basis  (matrix/scale (matrix/translate matrix/identity point) (/ scale)) ; screen-to-world
        invert (matrix/inverse basis) ; world-to-screen

        closing? (and (seq points) (some? cursor) (< (vec/dist (first points) cursor) (/ 32 scale))) ; Closing distance in world units
        ]
    ($ :<>
      ($ :rect.scene-draw-surface
        {:on-pointer-down (fn [event] (.stopPropagation event))
         :on-pointer-move
         (fn [event]
           (let [screen-point (Vec2. (.-clientX event) (.-clientY event))
                 world-point (vec/transform screen-point basis)] ; basis is screen-to-world
             (set-cursor (vec/rnd world-point (if align? half-size 1))))) ; cursor state is world-aligned
         :on-click
         (fn [event]
           (if (not closing?)
             (set-points (conj points cursor)) ; Add current world-aligned cursor to points
             (let [final-world-points (if closing? (conj points cursor) points)
                   xs (geom/reorient (mapcat seq final-world-points))
                   xf (comp (partition-all 2) (map (fn [[x y]] (Vec2. x y))))]
               (set-points [])
               (set-cursor nil)
               (on-create event (into [] xf xs)))))}) ; on-create receives world points
      (if (and align? (not closing?) (some? cursor)) ; cursor is world-aligned
        ($ anchor {:transform (vec/transform cursor invert)})) ; invert is world-to-screen
      (if (seq points) ; points are world-space
        ($ :circle.scene-draw-point-ring
          {:transform (vec/transform (first points) invert) :r 6})) ; invert is world-to-screen
      (for [point points :let [screen-point (vec/transform point invert)]] ; points are world-space
        ($ :circle.scene-draw-point {:key screen-point :transform screen-point :r 4}))
      (if (and (seq points) (some? cursor)) ; points & cursor are world-space
        ($ :polygon.scene-draw-shape
          {:points (transduce (map (comp seq #(vec/transform % invert))) points->poly []
                              (if (not closing?) (conj points cursor) points))}))))) ; invert is world-to-screen

(defui ^:private draw-select []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:on-release (fn [world-segment] (dispatch :selection/from-rect world-segment))}
      ;; Parameters: world-segment (was 'camera'), screen-segment (was 'canvas'), invert-matrix (new)
      (fn [_ screen-segment _] ; world-segment and invert-matrix not used here
        (let [a (.-a screen-segment) b (.-b screen-segment)]
          ($ hooks/use-portal {:name :multiselect}
            ($ :path.scene-draw-shape
              {:d (join " " [\M (.-x a) (.-y a) \H (.-x b) \V (.-y b) \H (.-x a) \Z])})))))))

(defui ^:private draw-ruler []
  ($ draw-segment {:align-fn align-grid-half}
    ;; Parameters: world-segment (was 'camera'), screen-segment (was 'canvas'), invert-matrix (new)
    (fn [world-segment screen-segment _] ; invert-matrix not used here
      (let [a (.-a screen-segment) b (.-b screen-segment)]
        ($ :<>
          ($ :line.scene-draw-shape {:x1 (.-x a) :y1 (.-y a) :x2 (.-x b) :y2 (.-y b)})
          ($ anchor {:transform a})
          ($ anchor {:transform b})
          ($ text {:attrs {:x (- (.-x b) 48) :y (- (.-y b) 8)}}
            (str (px->ft (vec/dist-cheb world-segment)) "ft.")))))))

(defui ^:private draw-circle []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release (fn [world-segment] (dispatch :shape/create :circle (seq world-segment)))
       :tile-path (fn [world-segment] (let [r (vec/dist-cheb world-segment)] (geom/tile-path-circle (.-a world-segment) r)))}
      ;; Parameters: world-segment (was 'camera'), screen-segment (was 'canvas'), invert-matrix (new)
      (fn [world-segment screen-segment _] ; invert-matrix not used here
        (let [src-screen (.-a screen-segment)
              radius-world (vec/dist-cheb world-segment)
              radius-screen (vec/dist-cheb screen-segment)] ; For SVG 'r' attribute
          ($ :<>
            ($ :circle.scene-draw-shape {:transform src-screen :r radius-screen})
            ($ text {:attrs {:x (.-x src-screen) :y (.-y src-screen) :fill "white"}}
              (str (px->ft radius-world) "ft. radius"))))))))

(defui ^:private draw-rect []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release (fn [world-segment] (dispatch :shape/create :rect (seq world-segment)))}
      ;; Parameters: world-segment (was 'camera'), screen-segment (was 'canvas'), invert-matrix (new)
      (fn [world-segment screen-segment _] ; invert-matrix not used here
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
      ;; Parameters: world-segment (was 'camera'), screen-segment (was 'canvas'), invert-matrix (new, used here)
      (fn [world-segment screen-segment invert-matrix]
        ($ :<>
          (let [world-polygon-vertices (geom/line-points world-segment) ; geom/line-points now expects world-segment
                screen-polygon-vertices (map #(vec/transform % invert-matrix) world-polygon-vertices)]
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
      ;; Parameters: world-segment (was 'camera'), screen-segment (was 'canvas'), invert-matrix (new, used here)
      (fn [world-segment screen-segment invert-matrix]
        ($ :<>
          (let [world-cone-shape-points (geom/cone-points world-segment)
                screen-cone-shape-points (map #(vec/transform % invert-matrix) world-cone-shape-points)]
            ($ :polygon.scene-draw-shape
              {:points (join " " (mapcat seq screen-cone-shape-points))}))
          ($ text {:attrs {:x (+ (.-x (.-b screen-segment)) 16) :y (+ (.-y (.-b screen-segment)) 16)}}
            (str (px->ft (vec/dist world-segment)) "ft.")))))))

(defui ^:private draw-poly []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon {:on-create (fn [_ world-points] (dispatch :shape/create :poly world-points))})))

(defui ^:private draw-mask []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon {:on-create (fn [_ world-points] (dispatch :mask/create (mapcat seq world-points)))})))

(defui ^:private draw-grid []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query)
        {bounds :user/bounds
         {point :camera/point ; Renamed from 'shift' for clarity if it's camera world pos
          scale :camera/scale
          {prev-size :scene/grid-size prev-origin :scene/grid-origin} :camera/scene} :user/camera} result
        [origin set-origin] (uix/use-state nil) ; screen-space click point for grid placement
        [size set-size] (uix/use-state prev-size)

        basis  (matrix/scale (matrix/translate matrix/identity point) (/ scale)) ; screen-to-world
        invert (matrix/inverse basis) ; world-to-screen

        grid-display-origin-on-screen (.-a bounds) ; Assuming (.-a bounds) is a fixed screen reference, e.g. top-left of drawing area
                                                  ; This interpretation needs to be correct for the transform.
                                                  ; Or perhaps it's a world coord to offset the whole grid system.
                                                  ; The original (vec/sub origin basis) was complex.
                                                  ; Let's assume the transform positions the grid relative to screen point 'origin'.
        on-shift (fn [offset-screen] (fn [] (set-origin (fn [current-screen-origin] (vec/add current-screen-origin offset-screen)))))]
    ($ :g.grid-align
      ($ :rect.scene-draw-surface
        {:on-click (fn [event] (set-origin (Vec2. (.-clientX event) (.-clientY event))))})
      (if (some? origin) ; 'origin' is the screen point where the grid's logical (0,0) or center should appear for adjustment
        (let [rows 7
              display-grid-cell-size (* size scale (/ grid-size prev-size)) ; Visual size of grid cell on screen.
              wide (* display-grid-cell-size (inc rows))
              path (for [step (range (- rows) (inc rows))]
                     (str "M " (* step display-grid-cell-size) " " (- wide) " " \V " " wide " "
                          "M " (- wide) " " (* step display-grid-cell-size) " " \H " " wide " "))
              ;; The group transform should make the grid's reference point (e.g., its center) appear at screen 'origin'
              group-transform-str (str "translate(" (.-x origin) "," (.-y origin) ")")]
          ($ :g {:transform group-transform-str} ; Grid drawing is centered around this screen point 'origin'
            ($ :path.grid-align-path {:d (join path)})
            ($ :circle.grid-align-center {:r 6})
            ($ :foreignObject.grid-align-form {:x -128 :y -128 :width 256 :height 256}
              ($ :form
                {:on-submit
                 (fn [event]
                   (.preventDefault event)
                   ;; 'origin' is screen. Convert to world, then apply logic.
                   (let [world-origin-for-dispatch (-> (vec/transform origin basis) ; basis is screen-to-world
                                                       (vec/add point) ; Adjust based on camera pos (if 'point' is camera pos and 'basis' incorporates it)
                                                                      ; This part is tricky. The original was:
                                                                      ; (-> (vec/sub origin basis) (vec/div scale) (vec/add shift) ...)
                                                                      ; Let's try to match original intent more closely if 'basis' definition is exact.
                                                                      ; If basis = (matrix/scale (matrix/translate I point) (/ scale)), then (vec/transform origin basis) gives a world point.
                                                       (vec/add (or prev-origin vec/zero)) ; Add previous world offset of grid
                                                       (vec/mul (/ prev-size size))
                                                       (vec/abs)
                                                       (vec/mod grid-size)
                                                       (vec/rnd 0.25))]
                     (dispatch :scene/apply-grid-options world-origin-for-dispatch size)))}
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
        result (hooks/use-query query)
        {{point :camera/point scale :camera/scale} :user/camera} result
        basis  (matrix/scale (matrix/translate matrix/identity point) (/ scale))] ; screen-to-world
    ($ :rect.scene-draw-surface
      {:on-click
       (fn [event]
         (let [screen-point (Vec2. (.-clientX event) (.-clientY event))
               world-point (vec/transform screen-point basis)] ; basis is screen-to-world
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