;; src/main/ogres/app/events.cljs
(ns ogres.app.events
  (:require [datascript.core :as ds]
            [clojure.set :refer [union difference]]
            [clojure.string :as str :refer [trim]] ; Aliased clojure.string to str
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.geom :as geom]
            [ogres.app.vec :as vec :refer [Vec2]]))

(def ^:private suffix-max-xf
  (map (fn [[label tokens]] [label (apply max (map :initiative/suffix tokens))])))

(def ^:private zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

(defn ^:private linear [dx dy rx ry]
  (fn [n] (+ (* (/ (- n dx) (- dy dx)) (- ry rx)) rx)))

(defn ^:private indexed
  "Returns a transducer which decorates each element with a decreasing
   negative index suitable for use as temporary ids in a DataScript
   transaction. Optionally receives an offset integer to begin counting and
   a step integer to create space between indexes."
  ([]
   (indexed 1 1))
  ([offset]
   (indexed offset 1))
  ([offset step]
   (map-indexed (fn [idx val] [(-> (* idx step) (+ offset) (* -1)) val]))))

(defn ^:private suffix-token-key
  "Returns a grouping key for the given token that will match other similarly
   identifiable tokens."
  [token]
  (let [{label :token/label {hash :image/hash} :token/image} token]
    [label hash]))

(defn ^:private suffixes
  "Returns a map of `{entity key => suffix}` for the given token entities.
   Each suffix represents a unique and stable identity for a token within
   the group of tokens by which it shares a label. Suffixes are intended to
   help decorate tokens that may otherwise be difficult to distinguish
   when they share the same image and label."
  [tokens]
  (let [groups (group-by suffix-token-key tokens)
        offset (into {} suffix-max-xf groups)]
    (loop [tokens tokens index {} result {}]
      (if (seq tokens)
        (let [token (first tokens)
              group (suffix-token-key token)]
          (if (or (= (count (groups group)) 1)
                  (:initiative/suffix token)
                  (contains? (:token/flags token) :player))
            (recur (rest tokens) index result)
            (recur (rest tokens)
                   (update index group inc)
                   (assoc result (:db/id token) (+ (offset group) (index group) 1)))))
        result))))

(defn ^:private to-precision [n p]
  (js/Number (.toFixed (js/Number.parseFloat n) p)))

(defn ^:private constrain [n min max]
  (clojure.core/max (clojure.core/min n max) min))

(defn ^:private mode-allowed? [mode type]
  (not (and (contains? #{:mask :mask-toggle :mask-remove :grid :note} mode)
            (not= type :host))))

(defn ^:private initiative-order [a b]
  (let [f (juxt :initiative/roll :db/id)]
    (compare (f b) (f a))))

(defn ^:private random-rolls
  "Returns a lazy infinite sequence of random integers in the domain
   of [start, end]. Each group of integers in the domain are guaranteed
   to be unique among each other."
  [start end]
  (sequence (mapcat shuffle) (repeat (range start (inc end)))))

(defn ^:private roll-token? [token]
  (let [{:keys [initiative/roll token/flags]} token]
    (and (not (contains? flags :player))
         (not (number? roll)))))

(defmulti event-tx-fn (fn [_db-val event-type & _args] event-type)) ; _db-val is unused in dispatch

(defmethod event-tx-fn :default [_db _event-type] [])

;; -- Local --
(defmethod event-tx-fn :user/select-panel
  [_db _event-type panel]
  [{:db/ident :user :panel/selected panel :panel/expanded true}])

(defmethod event-tx-fn :user/toggle-panel
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])]
    [{:db/ident :user :panel/expanded (not (get user :panel/expanded true))}]))

(defmethod event-tx-fn :user/change-label
  ([_db _event-type value]
   [{:db/ident :user :user/label value}])
  ([_db _event-type uuid value]
   [{:user/uuid uuid :user/label value}]))

(defmethod event-tx-fn :user/change-description
  ([_db _event-type value]
   [{:db/ident :user :user/description value}])
  ([_db _event-type uuid value]
   [{:user/uuid uuid :user/description value}]))

(defmethod event-tx-fn :user/change-image
  ([_db _event-type hash]
   [{:db/ident :user :user/image [:image/hash hash]}])
  ([_db _event-type uuid hash]
   [{:user/uuid uuid :user/image [:image/hash hash]}]))

(defmethod event-tx-fn :user/change-details
  ([_db _event-type label description]
   [{:db/ident :user :user/label label :user/description description}])
  ([_db _event-type uuid label description]
   [{:user/uuid uuid :user/label label :user/description description}]))

;; -- Camera --
(defn ^:private assoc-camera
  [db & kvs]
  (let [user (ds/entity db [:db/ident :user])]
    [(apply assoc {:db/id (:db/id (:user/camera user))} kvs)]))

(defmethod event-tx-fn :camera/change-label
  [_db _event-type label]
  [[:db.fn/call assoc-camera :camera/label label]])

(defmethod event-tx-fn :camera/remove-label
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])]
    [[:db/retract (:db/id (:user/camera user)) :camera/label]]))

(defmethod event-tx-fn :camera/translate
  [db _event-type delta]
  (let [{{camera-id :db/id point :camera/point scale :camera/scale} :user/camera} ; Renamed id to camera-id
        (ds/entity db [:db/ident :user])]
    [{:db/id camera-id :camera/point (vec/add (or point vec/zero) (vec/div delta (or scale 1)))}]))

(defmethod event-tx-fn :camera/change-mode
  [db _event-type mode]
  (let [user (ds/entity db [:db/ident :user])]
    (if (mode-allowed? mode (:user/type user))
      [{:db/id (:db/id (:user/camera user)) :camera/draw-mode mode}]
      [])))

(defmethod event-tx-fn :camera/zoom-change
  ([db event-keyword & args] ; Using event-keyword
   (let [user (ds/entity db [:db/ident :user])]
     (case (count args)
       0 [[:db.fn/call event-tx-fn event-keyword 1]] ; Pass event-keyword
       1 (let [[scale-val] args
               bounds (or (:user/bounds user) vec/zero-segment)]
           [[:db.fn/call event-tx-fn event-keyword scale-val (vec/midpoint bounds)]]) ; Pass event-keyword
       (let [[next-scale point] args
             {{camera-id :db/id current-scale :camera/scale camera-point :camera/point} :user/camera} user] ; Renamed id, scale, camera
         [{:db/id camera-id
           :camera/scale next-scale
           :camera/point
           (-> (vec/mul point (/ next-scale (or current-scale 1)))
               (vec/sub point)
               (vec/div next-scale)
               (vec/add (or camera-point vec/zero)))}])))))

(defmethod event-tx-fn :camera/zoom-delta
  [db _event-type mx my delta trackpad?] ; Changed 'data' to 'db'
  (let [user (ds/entity db [:db/ident :user])
        bound (or (:user/bounds user) vec/zero-segment)
        scale-fn (linear -400 400 -0.50 0.50) ; Renamed 'scale' to 'scale-fn'
        delta-amount (if trackpad? (scale-fn (* -1 8 delta)) (scale-fn (* -1 2 delta)))
        point (vec/sub (Vec2. mx my) (.-a bound))
        new-scale (-> (:camera/scale (:user/camera user)) (or 1)
                      (js/Math.log) (+ delta-amount) (js/Math.exp)
                      (to-precision 2) (constrain 0.15 4))]
    [[:db.fn/call event-tx-fn :camera/zoom-change new-scale point]]))

(defmethod event-tx-fn :camera/zoom-in
  [db _event-type]
  (let [user   (ds/entity db [:db/ident :user])
        camera (:user/camera user)
        prev-scale   (or (:camera/scale camera) 1)
        next-scale   (reduce (fn [n s] (if (> s prev-scale) (reduced s) n)) prev-scale zoom-scales)]
    [[:db.fn/call event-tx-fn :camera/zoom-change next-scale]]))

(defmethod event-tx-fn :camera/zoom-out
  [db _event-type]
  (let [user   (ds/entity db [:db/ident :user])
        camera (:user/camera user)
        prev-scale   (or (:camera/scale camera) 1)
        next-scale   (reduce (fn [n s] (if (< s prev-scale) (reduced s) n)) prev-scale (reverse zoom-scales))]
    [[:db.fn/call event-tx-fn :camera/zoom-change next-scale]]))

(defmethod event-tx-fn :camera/zoom-reset
  [_db _event-type]
  [[:db.fn/call event-tx-fn :camera/zoom-change 1]])

;; -- Scenes --
(defmethod event-tx-fn :scenes/create
  [_db _event-type]
  [[:db/add -1 :db/ident :root]
   [:db/add -1 :root/scenes -2]
   {:db/id -2 :db/empty true} ; Scene entity
   {:db/id -4 :camera/scene [:db/id -2] :camera/point vec/zero} ; Camera entity, linking to scene by temp id
   {:db/id [:db/ident :user] :user/camera -4 :user/cameras [-4]}]) ; Update user

(defmethod event-tx-fn :scenes/change
  [_db _event-type id]
  [{:db/id [:db/ident :user] :user/camera id}])

(defmethod event-tx-fn :scenes/remove
  [db _event-type camera-eid-to-remove] ; 'id' is the entity id of the camera
  (let [camera-to-remove  (ds/entity db camera-eid-to-remove)
        scene-to-remove-eid (:db/id (:camera/scene camera-to-remove))
        user                (ds/entity db [:db/ident :user])
        session             (ds/entity db [:db/ident :session])
        all-user-cameras    (:user/cameras user)]
    (cond
      (= (count all-user-cameras) 1) ; If only one camera/scene exists for the host
      (let [new-scene-eid -200 ; Temporary IDs for the new default scene/camera
            new-camera-eid -201]
        (into [[:db/retractEntity camera-eid-to-remove]
               [:db/retractEntity scene-to-remove-eid]
               ;; Add new default scene
               {:db/id new-scene-eid, :db/empty true}
               ;; Update root to point to new default scene
               [:db/add [:db/ident :root] :root/scenes new-scene-eid]
               ;; Add new default camera for host
               {:db/id new-camera-eid, :camera/scene new-scene-eid, :camera/point vec/zero}
               ;; Update host's current camera and camera list
               {:db/id [:db/ident :user], :user/camera new-camera-eid, :user/cameras [new-camera-eid]}]
              ;; Update connected users to also point to this new default scene/camera
              (for [[temp-cam-id conn-entity] (sequence (indexed -300 1) (:session/conns session))]
                {:db/id (:db/id conn-entity) ; Assuming conn-entity is the full entity map
                 :user/camera temp-cam-id
                 :user/cameras [{:db/id temp-cam-id
                                 :camera/scene new-scene-eid
                                 :camera/point vec/zero
                                 :camera/scale 1}]})))

      (= camera-eid-to-remove (:db/id (:user/camera user))) ; If host removes their currently active camera/scene
      (let [remaining-cameras (filter #(not= (:db/id %) camera-eid-to-remove) all-user-cameras)
            next-host-camera (first remaining-cameras)
            next-host-camera-eid (:db/id next-host-camera)
            next-host-scene-eid (:db/id (:camera/scene next-host-camera))]
        (into [[:db/retractEntity camera-eid-to-remove]
               [:db/retractEntity scene-to-remove-eid]
               {:db/id [:db/ident :user] :user/camera next-host-camera-eid}] ; Set new active camera for host
              (for [[temp-cam-id conn-entity] (sequence (indexed) (:session/conns session))
                    :let [existing-cam-for-next-scene (first (filter #(= (:db/id (:camera/scene %)) next-host-scene-eid)
                                                                     (:user/cameras conn-entity)))
                          final-cam-id (or (:db/id existing-cam-for-next-scene) temp-cam-id)]]
                {:db/id (:db/id conn-entity)
                 :user/camera final-cam-id
                 :user/cameras [{:db/id final-cam-id
                                 :camera/scene next-host-scene-eid
                                 :camera/point vec/zero
                                 :camera/scale 1}]})))
      :else ; A non-active scene is removed by the host
      [[:db/retractEntity camera-eid-to-remove]
       [:db/retractEntity scene-to-remove-eid]])))

;; -- Scene Images --
(defmethod event-tx-fn :scene-images/create-many
  [_db _event-type images]
  (into [{:db/id [:db/ident :root]
          :root/scene-images
          (mapv (fn [[img-data _thumb-data]] ; Using mapv for vector
                  (let [{:keys [hash name size width height]} img-data]
                    {:db/id [:image/hash hash] ; Ensure image entities are identifiable
                     :image/hash hash :image/name name :image/size size
                     :image/width width :image/height height}))
                images)}]
        cat
        (for [[image thumbnail] images]
          (if (= (:hash image) (:hash thumbnail))
            [{:db/id [:image/hash (:hash image)] :image/thumbnail [:image/hash (:hash image)]}]
            [{:db/id [:image/hash (:hash thumbnail)] :image/hash (:hash thumbnail) :image/name (:name thumbnail)
              :image/size (:size thumbnail) :image/width (:width thumbnail) :image/height (:height thumbnail)}
             {:db/id [:image/hash (:hash image)] :image/thumbnail [:image/hash (:hash thumbnail)]}]))))

(defmethod event-tx-fn :scene-images/remove
  [_db _event-type image-hash thumb-hash]
  (if (= image-hash thumb-hash)
    [[:db/retractEntity [:image/hash image-hash]]]
    [[:db/retractEntity [:image/hash image-hash]]
     [:db/retractEntity [:image/hash thumb-hash]]]))

;; -- Scene --
(defn ^:private assoc-scene
  "Helper transaction function to associate key-value pairs with the current scene."
  [db & kvs]
  (let [user (ds/entity db [:db/ident :user])
        scene-id (:db/id (:camera/scene (:user/camera user)))]
    [(apply assoc {:db/id scene-id} kvs)]))

(defmethod event-tx-fn :scene/change-image
  [_db _event-type hash]
  [[:db.fn/call assoc-scene :scene/image [:image/hash hash]]])

(defmethod event-tx-fn :scene/change-grid-size
  [_db _event-type size]
  [[:db.fn/call assoc-scene :scene/grid-size size]])

(defmethod event-tx-fn :scene/apply-grid-options
  [db _event-type origin size]
  (let [user-entity (ds/entity db [:db/ident :user])
        camera-data (:user/camera user-entity)
        scene-data (:camera/scene camera-data)
        camera-id (:db/id camera-data)
        camera-point (:camera/point camera-data)
        scene-id (:db/id scene-data)
        prev-origin (:scene/grid-origin scene-data)]
    [{:db/id camera-id
      :camera/draw-mode :select
      :camera/point (vec/sub (vec/add camera-point (or prev-origin vec/zero)) origin)
      :camera/scene {:db/id scene-id
                     :scene/grid-size size
                     :scene/grid-origin origin}}]))

(defmethod event-tx-fn :scene/reset-grid-origin
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])
        scene-id (:db/id (:camera/scene (:user/camera user)))]
    [[:db.fn/call assoc-camera :camera/draw-mode :select]
     [:db/retract scene-id :scene/grid-origin]]))

(defmethod event-tx-fn :scene/retract-grid-size
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])
        scene-id (:db/id (:camera/scene (:user/camera user)))]
    [[:db/retract scene-id :scene/grid-size]]))

(defmethod event-tx-fn :scene/toggle-show-grid
  [_db _event-type value]
  [[:db.fn/call assoc-scene :scene/show-grid value]])

(defmethod event-tx-fn :scene/toggle-dark-mode
  [_db _event-type enabled]
  [[:db.fn/call assoc-scene :scene/dark-mode enabled]])

(defmethod event-tx-fn :scene/toggle-grid-align
  [_db _event-type enabled]
  [[:db.fn/call assoc-scene :scene/grid-align enabled]])

(defmethod event-tx-fn :scene/toggle-object-outlines
  [_db _event-type enabled]
  [[:db.fn/call assoc-scene :scene/show-object-outlines enabled]])

(defmethod event-tx-fn :scene/change-lighting
  [_db _event-type value]
  [[:db.fn/call assoc-scene :scene/lighting value]])

;; --- Objects ---
(defmethod event-tx-fn :objects/translate
  [_db _event-type id delta]
  [[:db.fn/call event-tx-fn :objects/translate-many #{id} delta]])

(def ^:private translate-many-select
  [:db/id :object/type :object/point :shape/points :token/size])

(defmethod event-tx-fn :objects/translate-many
  [db _event-type idxs delta]
  (let [user-entity (ds/entity db [:db/ident :user])
        align? (-> user-entity :user/camera :camera/scene :scene/grid-align)]
    (into [[:db/retract [:db/ident :user] :user/dragging]]
          (for [entity (ds/pull-many db translate-many-select idxs)
                :let [{id :db/id current-point :object/point} entity]]
            (cond
              (and align? (= (:object/type entity) :token/token))
              (let [bounds (vec/rnd (vec/add (geom/object-bounding-rect entity) delta) grid-size)]
                {:db/id id :object/point (vec/midpoint bounds)})
              (and align? (not= (:object/type entity) :note/note))
              (let [round-fn (geom/object-alignment entity)]
                {:db/id id :object/point (vec/rnd (vec/add current-point delta) round-fn)})
              :else
              {:db/id id :object/point (vec/add current-point delta)})))))

(defmethod event-tx-fn :objects/translate-selected
  [db _event-type delta]
   (let [user-entity (ds/entity db [:db/ident :user])
         selected-objects (:camera/selected (:user/camera user-entity))]
     [[:db.fn/call event-tx-fn :objects/translate-many (into #{} (map :db/id) selected-objects) delta]]))

(defmethod event-tx-fn :objects/select
  [db _event-type id modify?]
  (let [object-to-select (ds/entity db id)
        user-entity (ds/entity db [:db/ident :user])
        {{camera-id :db/id selected-collection :camera/selected} :user/camera} user-entity
        object-eid (:db/id object-to-select)]
    (vec (cons
          [:db/retract [:db/ident :user] :user/dragging]
          (cond
            (not modify?)
            ;; If not modifying, clear previous and set this one (if object-eid is valid)
            (if object-eid
              [[:db/retract camera-id :camera/selected]
               [:db/add camera-id :camera/selected object-eid]]
              [[:db/retract camera-id :camera/selected]])

            modify? ; If modifying (toggling in a multi-select scenario for :camera/selected which is card-many)
            (if object-eid
              (if (some #(= object-eid (:db/id %)) (if (coll? selected-collection) selected-collection [selected-collection]))
                [[:db/retract camera-id :camera/selected object-eid]] ; Object is selected, so retract it
                [[:db/add camera-id :camera/selected object-eid]])   ; Object not selected, so add it
              []) ; No valid object-eid to modify with

            :else []))))) ; Default case if needed

(defmethod event-tx-fn :objects/toggle-hidden
  [db _event-type id]
  (let [entity (ds/entity db id)]
    [[:db/add id :object/hidden (not (:object/hidden entity))]]))

(defmethod event-tx-fn :objects/change-hidden
  [_db _event-type idxs value]
  (for [id idxs]
    [:db/add id :object/hidden value]))

(defmethod event-tx-fn :objects/remove
  [_db _event-type idxs]
  (for [id idxs]
    [:db/retractEntity id]))

(defmethod event-tx-fn :objects/update
  [_db _event-type idxs attr value]
  (for [id idxs]
    (assoc {:db/id id} attr value)))

;; --- Tokens ---
(defmethod event-tx-fn :token/create
  [db _event-type point hash]
  (let [user-entity (ds/entity db [:db/ident :user])
        {{camera-id :db/id camera-shift :camera/point camera-scale :camera/scale
          {scene-id :db/id align? :scene/grid-align} :camera/scene} :user/camera} user-entity
        target-point (vec/add (vec/div point (or camera-scale 1)) (or camera-shift vec/zero))]
    [(cond-> {:db/id -1 :object/type :token/token}
       (some? hash) (assoc :token/image [:image/hash hash]) ; Use lookup ref
       (not align?) (assoc :object/point target-point)
       align?       (assoc :object/point
                           (-> (vec/shift target-point (- half-size))
                               (vec/rnd grid-size)
                               (vec/shift half-size))))
     {:db/id camera-id
      :camera/selected -1
      :camera/draw-mode :select
      :camera/scene {:db/id scene-id :scene/tokens -1}}]))

(defmethod event-tx-fn :token/change-flag
  [db _event-type idxs flag add?]
  (let [tokens (ds/pull-many db [:db/id :token/flags] idxs)]
    (for [{:keys [db/id token/flags] :or {flags #{}}} tokens]
      {:db/id id :token/flags ((if add? conj disj) flags flag)})))

(defmethod event-tx-fn :token/change-label
  [_db _event-type idxs value]
  (for [id idxs]
    {:db/id id :token/label (trim value)}))

(defmethod event-tx-fn :token/change-size
  [_db _event-type idxs radius]
  (for [id idxs]
    {:db/id id :token/size radius}))

(defmethod event-tx-fn :token/change-light
  [_db _event-type idxs radius]
  (for [id idxs]
    {:db/id id :token/light radius}))

(defmethod event-tx-fn :token/change-aura
  [_db _event-type idxs radius]
  (for [id idxs]
    {:db/id id :token/aura-radius radius}))

(defmethod event-tx-fn :token/change-dead
  [_db _event-type idxs add?]
  [[:db.fn/call event-tx-fn :token/change-flag idxs :dead add?]
   (if add?
     [:db.fn/call event-tx-fn :initiative/toggle idxs false])])

(defmethod event-tx-fn :shape/create
  [_db _event-type type [src & points]]
  [{:db/id -1
    :object/type (keyword "shape" type) ; Ensure :shape namespace
    :object/point src
    :shape/points (into [] (map (fn [vrt] (vec/sub vrt src))) points)}
   [:db.fn/call assoc-camera :camera/draw-mode :select :camera/selected -1]
   [:db.fn/call assoc-scene :scene/shapes -1]])

(defmethod event-tx-fn :user/change-bounds ; Was :bounds/change in original, matching dispatch
  [_db _event-type bounds]
  [{:db/id [:db/ident :user] :user/bounds bounds}]) ; Use lookup ref for :user

(defmethod event-tx-fn :selection/from-rect
  [db _event-type rect]
  (let [root-entity (ds/entity db [:db/ident :root])
        {{{{tokens :scene/tokens shapes :scene/shapes notes  :scene/notes} :camera/scene
           camera-id :db/id} :user/camera
          user-type :user/type} :root/user
         {session-conns :session/conns} :root/session} root-entity
        bounds-rect (geom/bounding-rect (seq rect)) ; Renamed 'bounds'
        locked-eids (into #{} (comp (mapcat :user/dragging) (map :db/id)) session-conns)] ; Renamed 'locked'
    [{:db/id camera-id
      :camera/draw-mode :select
      :camera/selected                           ; This needs to be a collection of lookup refs
      (into []
            (comp (filter (fn [entity]
                            (let [obj-eid (:db/id entity)
                                  object-bounds (geom/object-bounding-rect entity)]
                              (and (geom/rect-intersects-rect object-bounds bounds-rect)
                                   (not (locked-eids obj-eid))
                                   (or (= user-type :host) (not (:object/locked entity)))
                                   (or (= user-type :host) (not (:object/hidden entity)))))))
                  (map (fn [entity] {:db/id (:db/id entity)}))) ; Map to lookup refs
            (concat shapes tokens notes))}]));

(defmethod event-tx-fn :selection/clear
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])]
    [[:db/retract (:db/id (:user/camera user)) :camera/selected]]))

(defmethod event-tx-fn :selection/remove
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])
        selected-items (:camera/selected (:user/camera user))]
    ;; :camera/selected can be single or many based on schema and usage
    ;; This 'for' handles both if selected-items is made a collection
    (for [entity (if (coll? selected-items) selected-items [selected-items]) :when (some? (:db/id entity))]
      [:db/retractEntity (:db/id entity)])))

(defmethod event-tx-fn :initiative/toggle
  [db _event-type idxs adding?]
  (let [user   (ds/entity db [:db/ident :user])
        scene-id  (:db/id (:camera/scene (:user/camera user)))
        select-pattern [:db/id {:token/image [:image/hash]} [:token/flags :default #{}] :initiative/suffix :token/label]
        result (ds/pull db [{:scene/initiative select-pattern}] scene-id)
        tokens-to-change (into #{} (map :db/id) (ds/pull-many db select-pattern idxs)) ; Get EIDs
        existing-initiative-eids (into #{} (map :db/id) (:scene/initiative result))]
    (if adding?
      (let [all-eids (union existing-initiative-eids tokens-to-change)
            all-tokens (ds/pull-many db select-pattern (vec all-eids)) ; Pull full entities for suffixes
            sffxs (suffixes all-tokens)]
        [{:db/id scene-id
          :scene/initiative (into [] (map (fn [eid]
                                            (if-let [suffix (sffxs eid)]
                                              {:db/id eid :initiative/suffix suffix}
                                              {:db/id eid})))
                                  all-eids)}])
      ;; Removing
      (apply vector cat ; Ensure a flat vector of transaction statements
             (for [eid-to-remove tokens-to-change]
               [[:db/retract eid-to-remove :initiative/suffix]
                [:db/retract eid-to-remove :initiative/roll]
                [:db/retract eid-to-remove :initiative/health]
                [:db/retract scene-id :scene/initiative eid-to-remove]
                [:db/retract scene-id :initiative/played eid-to-remove]])))))

(defmethod event-tx-fn :initiative/next
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])
        scene-entity (:camera/scene (:user/camera user)) ; scene-entity is the full entity map
        scene-id (:db/id scene-entity)
        {current-rounds :initiative/rounds
         initiative-coll :scene/initiative ; collection of entities or refs
         played-coll :initiative/played} scene-entity] ; collection of entities or refs
    (if (some? current-rounds)
      (let [initiative-set (set (map :db/id initiative-coll)) ; Get EIDs
            played-set (set (map :db/id played-coll))         ; Get EIDs
            remaining-eids (difference initiative-set played-set)
            ;; Re-pull entities to sort them, or ensure initiative-coll is already sorted if needed
            remaining-entities (ds/pull-many db [:db/id :initiative/roll] (vec remaining-eids))
            next-turn-entity (first (sort initiative-order remaining-entities))]
        (if (some? next-turn-entity)
          [{:db/id scene-id
            :initiative/turn (:db/id next-turn-entity)
            :initiative/played [[:db/add scene-id :initiative/played (:db/id next-turn-entity)]]}] ; Add to :db.cardinality/many
          [[:db/retract scene-id :initiative/played]
           [:db/retract scene-id :initiative/turn]
           {:db/id scene-id :initiative/rounds (inc current-rounds)}]))
      [{:db/id scene-id :initiative/rounds 1}])))

(defmethod event-tx-fn :initiative/mark
  [db _event-type entity-id-to-mark] ; Renamed 'id' to 'entity-id-to-mark'
  (let [user (ds/entity db [:db/ident :user])
        scene-entity (:camera/scene (:user/camera user))
        scene-id (:db/id scene-entity)]
    [{:db/id scene-id
      :initiative/turn entity-id-to-mark ; Assuming :initiative/turn is :db.cardinality/one :db.type/ref
      :initiative/played [[:db/add scene-id :initiative/played entity-id-to-mark]] ; Add to :db.cardinality/many
      :initiative/rounds (max (or (:initiative/rounds scene-entity) 0) 1)}]))

(defmethod event-tx-fn :initiative/unmark
  [db _event-type entity-id-to-unmark] ; Renamed 'id'
  (let [user (ds/entity db [:db/ident :user])
        scene-entity (:camera/scene (:user/camera user))
        scene-id (:db/id scene-entity)]
    [[:db/retract scene-id :initiative/played entity-id-to-unmark]
     (if (= (:db/id (:initiative/turn scene-entity)) entity-id-to-unmark)
       [:db/retract scene-id :initiative/turn])]))

(defmethod event-tx-fn :initiative/change-roll
  [_db _event-type entity-id roll-val] ; Renamed 'id', 'roll'
  (let [parsed-val (.parseFloat js/window roll-val)] ; Renamed 'parsed'
    (cond
      (or (nil? roll-val) (= roll-val ""))
      [[:db/retract entity-id :initiative/roll]]
      (.isNaN js/Number parsed-val)
      []
      :else
      [{:db/id entity-id :initiative/roll parsed-val}])))

(defmethod event-tx-fn :initiative/roll-all
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])
        {{{initiative-tokens :scene/initiative} :camera/scene} :user/camera} user ; Renamed 'tokens'
        target-eids (sequence (comp (filter roll-token?) (map :db/id)) initiative-tokens)] ; Renamed 'idxs'
    (for [[eid roll-val] (zipmap target-eids (random-rolls 1 20))] ; Renamed 'id', 'roll'
      {:db/id eid :initiative/roll roll-val})))

(defmethod event-tx-fn :initiative/change-health
  [db _event-type entity-id f value-str] ; Renamed 'id', 'value'
  (let [parsed-val (.parseFloat js/window value-str)] ; Renamed 'parsed'
    (if (.isNaN js/Number parsed-val)
      []
      (let [{current-health :initiative/health} (ds/entity db entity-id)] ; Renamed 'health'
        [{:db/id entity-id :initiative/health (f (or current-health 0) parsed-val)}]))))

(defmethod event-tx-fn :initiative/leave
  [db _event-type]
  (let [user (ds/entity db [:db/ident :user])
        scene-entity (:camera/scene (:user/camera user))
        scene-id (:db/id scene-entity)]
    (apply concat
           [[:db/retract scene-id :scene/initiative]
            [:db/retract scene-id :initiative/turn]
            [:db/retract scene-id :initiative/played]
            [:db/retract scene-id :initiative/rounds]]
           (for [{obj-id :db/id} (:scene/initiative scene-entity)] ; Renamed 'id' to 'obj-id'
             [[:db/retract obj-id :initiative/roll]
              [:db/retract obj-id :initiative/health]
              [:db/retract obj-id :initiative/suffix]]))))

;; --- Token Images ---
(defmethod event-tx-fn :token-images/create-many
  [_db _event-type images scope]
  (into [{:db/id [:db/ident :root]
          :root/token-images
          (mapv (fn [[img-data _thumb-data]]
                  (let [{:keys [hash name size width height]} img-data]
                    {:db/id [:image/hash hash] ; Define entity with lookup ref
                     :image/hash hash :image/name name :image/size size
                     :image/scope scope :image/width width :image/height height}))
                images)}]
        cat
        (for [[image thumbnail] images]
          (if (= (:hash image) (:hash thumbnail))
            [{:db/id [:image/hash (:hash image)] :image/thumbnail [:image/hash (:hash image)]}]
            [{:db/id [:image/hash (:hash thumbnail)] :image/hash (:hash thumbnail) :image/name (:name thumbnail)
              :image/size (:size thumbnail) :image/width (:width thumbnail) :image/height (:height thumbnail)}
             {:db/id [:image/hash (:hash image)] :image/thumbnail [:image/hash (:hash thumbnail)]}]))))

(defmethod event-tx-fn :token-images/change-scope
  [_db _event-type hash scope]
  [[:db/add [:image/hash hash] :image/scope scope]]) ; Use lookup ref for the image entity

(defmethod event-tx-fn :token-images/remove
  [_db _event-type image-hash thumb-hash] ; Renamed
  (if (= image-hash thumb-hash)
    [[:db/retractEntity [:image/hash image-hash]]]
    [[:db/retractEntity [:image/hash image-hash]]
     [:db/retractEntity [:image/hash thumb-hash]]]))

(defmethod event-tx-fn :token-images/remove-all
  []
  [[:db/retract [:db/ident :root] :root/token-images]])

(defmethod event-tx-fn :token-images/change-thumbnail
  [_db _event-type hash thumb-data rect] ; Renamed 'thumb' to 'thumb-data'
  [{:db/id [:image/hash hash] ; Target entity by lookup ref
    :image/thumbnail-rect rect
    :image/thumbnail ; This should be a ref to the thumbnail entity
    ;; Assuming thumb-data contains info to create/identify thumbnail entity
    {:db/id [:image/hash (:hash thumb-data)] ; Define the thumbnail entity being referenced
     :image/hash (:hash thumb-data)
     :image/size (.-size (:data thumb-data)) ; Assuming :data key holds attributes
     :image/width (:width thumb-data)
     :image/height (:height thumb-data)}}])

;; --- Masks ---
(defmethod event-tx-fn :scene/mask
  [_db _event-type]
  [[:db.fn/call assoc-scene :scene/masked true]])

(defmethod event-tx-fn :scene/reveal
  [_db _event-type]
  [[:db.fn/call assoc-scene :scene/masked false]])

(defmethod event-tx-fn :mask/create
  [_db _event-type vecs]
  ;; Assuming :scene/masks is a to-many attribute that stores mask component entities
  [[:db.fn/call assoc-scene :scene/masks {:db/id -1 :mask/vecs vecs}]]) ; Add as a new component

(defmethod event-tx-fn :mask/toggle
  [_db _event-type id state]
  [{:db/id id :mask/enabled? state}])

(defmethod event-tx-fn :mask/remove
  [_db _event-type id]
  [[:db/retractEntity id]])

;; -- Session --
(defmethod event-tx-fn :session/request
  [_db _event-type]
  [;; This structure implies component entities. Ensure schema matches.
   {:db/id [:db/ident :root] :root/session
    {:db/id [:db/ident :session] :session/host
     {:db/id [:db/ident :user] :session/status :connecting :panel/selected :lobby}}}])

(defmethod event-tx-fn :session/join
  [_db _event-type]
  [{:db/id [:db/ident :user] :session/status :connecting}])

(defmethod event-tx-fn :session/close
  [_db _event-type]
  [{:db/id [:db/ident :user] :session/status :disconnected}
   [:db/retract [:db/ident :session] :session/host]
   [:db/retract [:db/ident :session] :session/conns]])

(defmethod event-tx-fn :session/toggle-share-cursors
  [_db _event-type enabled]
  [{:db/id [:db/ident :session] :session/share-cursors enabled}])

(defmethod event-tx-fn :session/change-status
  [_db _event-type status-code] ; Renamed 'status' to 'status-code'
  (let [statuses {0 :connecting 1 :connected 2 :disconnecting 3 :disconnected}]
    [{:db/id [:db/ident :user] :session/status (statuses status-code)}]))

(defmethod event-tx-fn :session/toggle-share-my-cursor
  [_db _event-type enabled]
  [{:db/id [:db/ident :user] :user/share-cursor enabled}])

(defmethod event-tx-fn :session/focus ; This is a complex operation
  [db _event-type]
  (let [select-w [:camera/scene [:camera/point :default vec/zero] [:camera/scale :default 1]]
        select-l [:db/id [:user/bounds :default vec/zero-segment]
                  {:user/cameras [:camera/scene] :user/camera select-w}]
        select-s [{:session/host select-l} {:session/conns select-l}]
        session-data   (ds/pull db select-s [:db/ident :session]) ; Renamed 'result'
        {{host-bounds :user/bounds
          {host-point :camera/point} :user/camera ; Renamed 'point'
          host-camera :user/camera} :session/host ; Renamed 'host'
         conn-entities :session/conns} session-data ; Renamed 'conns'
        host-scale (:camera/scale host-camera)
        center-point (vec/add host-point (vec/div (vec/midpoint host-bounds) host-scale))] ; Renamed 'center'
    (->> (for [[temp-id conn-entity] (sequence (indexed) conn-entities) ; Renamed 'next', 'conn'
               :let [prev-cam (->> (:user/cameras conn-entity)
                                   (filter (fn [cam] ; Renamed 'conn' to 'cam' in fn
                                             (= (:db/id (:camera/scene cam))
                                                (:db/id (:camera/scene host-camera)))))
                                   (first))
                     next-cam-id  (or (:db/id prev-cam) temp-id) ; Renamed 'next'
                     target-point (vec/sub center-point (vec/div (vec/midpoint (:user/bounds conn-entity)) host-scale))]] ; Renamed 'point'
           [;; Update conn-entity's active camera and camera list
            {:db/id (:db/id conn-entity) :user/camera next-cam-id :user/cameras [next-cam-id]} ; Assuming user/cameras is a list of refs/components
            ;; Define/update the camera entity itself
            {:db/id next-cam-id
             :camera/point target-point
             :camera/scale host-scale
             :camera/scene (:db/id (:camera/scene host-camera))}]) ; Ensure this is a ref
         (into [] cat))))

;; -- Clipboard --
(def ^:private clipboard-copy-attrs
  [:object/point :object/type :object/hidden :object/locked
   :note/icon :note/label :note/description
   :shape/points :shape/color :shape/pattern
   :token/label :token/flags :token/light :token/size :token/aura-radius :token/image])

(def ^:private clipboard-copy-select
  [{:user/camera
    [{:camera/selected ; This should pull entities or their EIDs
      (into clipboard-copy-attrs [:db/id {:token/image [:image/hash]}])}]}])

(defmethod event-tx-fn :clipboard/copy
  ([_db event-keyword] ; Using consistent arg names
   [[:db.fn/call event-tx-fn event-keyword false]])
  ([db _event-keyword cut?]
   (let [pulled-data (ds/pull db clipboard-copy-select [:db/ident :user]) ; Renamed 'result'
         selected-for-copy (:camera/selected (:user/camera pulled-data)) ; Renamed 'copied'
         data-to-copy (into [] (map #(select-keys % clipboard-copy-attrs)) selected-for-copy)] ; Renamed 'copies'
     (cond-> []
       (seq data-to-copy) (conj {:db/id [:db/ident :user] :user/clipboard data-to-copy})
       cut?         (into (for [{entity-id :db/id} selected-for-copy] ; Renamed 'id'
                            [:db/retractEntity entity-id]))))))

(def ^:private clipboard-paste-select
  [{:root/user
    [[:user/clipboard :default []]
     [:user/bounds :default vec/zero-segment]
     {:user/camera
      [:db/id
       [:camera/scale :default 1]
       [:camera/point :default vec/zero]
       {:camera/scene
        [:db/id
         [:scene/grid-align :default false]]}]}]}
   {:root/token-images [:image/hash]}])

(defmethod event-tx-fn :clipboard/paste
  [db _event-type] ; Changed 'data' to 'db'
  (let [pulled-data (ds/pull db clipboard-paste-select [:db/ident :root]) ; Renamed 'result'
        {{clipboard-items :user/clipboard ; Renamed 'clipboard'
          screen-bounds :user/bounds ; Renamed 'screen'
          {camera-id :db/id ; Renamed 'camera'
           current-scale :camera/scale ; Renamed 'scale'
           camera-point :camera/point ; Renamed 'point'
           {scene-id :db/id align-grid? :scene/grid-align} ; Renamed 'scene', 'align?'
           :camera/scene} :user/camera} :root/user
         token-image-hashes :root/token-images} pulled-data ; Renamed 'images'
        available-image-hashes (into #{} (map :image/hash) token-image-hashes) ; Renamed 'hashes'
        clipboard-bounds (transduce (mapcat geom/object-bounding-rect) geom/bounding-rect-rf clipboard-items) ; Renamed 'bound'
        delta-offset (vec/sub ; Renamed 'delta'
                      (vec/add camera-point (vec/div (vec/midpoint screen-bounds) current-scale))
                      (vec/midpoint clipboard-bounds))]
    (for [[temp-id item-to-copy] (sequence (indexed) clipboard-items) ; Renamed 'idx', 'copy'
          :let [{src-point :object/point item-type :object/type} item-to-copy ; Renamed 'src', 'type'
                image-ref-hash (:image/hash (:token/image item-to-copy)) ; Renamed 'hash'
                final-item-type (keyword (namespace item-type)) ; Renamed 'type'
                ;; Prepare data for the new entity
                entity-data (cond-> (assoc item-to-copy :db/id temp-id :object/point (vec/add src-point delta-offset))
                              align-grid? ; Renamed 'align?'
                              (assoc :object/point (vec/rnd (vec/add src-point delta-offset) grid-size))
                              (and (= final-item-type :token) (available-image-hashes image-ref-hash)) ; Use available-image-hashes
                              (assoc :token/image [:image/hash image-ref-hash]) ; Use lookup ref
                              (and (= final-item-type :token) align-grid?)
                              (assoc :object/point
                                     (let [original-bounds (geom/object-bounding-rect item-to-copy)
                                           aligned-bounds (vec/rnd (vec/add original-bounds delta-offset) grid-size)]
                                       (vec/midpoint aligned-bounds))))]]
      ;; Associate the new entity with the current scene via the camera entity
      {:db/id camera-id
       :camera/selected temp-id ; Select the newly pasted item
       :camera/scene ; This needs to add to a to-many collection on the scene entity
       (cond-> {:db/id scene-id} ; Target the scene entity
         (= final-item-type :note)  (assoc :scene/notes entity-data) ; Add as component if schema allows
         (= final-item-type :shape) (assoc :scene/shapes entity-data)
         (= final-item-type :token) (assoc :scene/tokens entity-data))})))

;; -- Shortcuts --
(defmethod event-tx-fn :shortcut/escape
  [db _event-type] ; Changed 'data' to 'db'
  (let [user (ds/entity db [:db/ident :user])
        camera-id   (:db/id (:user/camera user))] ; Renamed 'id'
    [{:db/id camera-id :camera/draw-mode :select}
     [:db/retract camera-id :camera/selected]]))

;; -- Dragging --
(defmethod event-tx-fn :drag/start
  [_db _event-type id]
  [{:db/id [:db/ident :user] :user/dragging id}]) ; Use lookup ref

(defmethod event-tx-fn :drag/start-selected
  [db _event-type] ; Changed 'data' to 'db'
  (let [user-entity (ds/entity db [:db/ident :user]) ; Renamed 'result'
        {{selected-items :camera/selected} :user/camera} user-entity] ; Renamed 'selected'
    [{:db/id [:db/ident :user] :user/dragging (mapv :db/id selected-items)}])) ; mapv for vector, ensure refs

(defmethod event-tx-fn :drag/end
  [_db _event-type]
  [[:db/retract [:db/ident :user] :user/dragging]])

;; -- Notes --
(defmethod event-tx-fn :note/create
  [db _event-type point] ; Changed 'data' to 'db'
  (let [user (ds/entity db [:db/ident :user])
        {screen-bounds :user/bounds ; Renamed 'bounds'
         {camera-id :db/id ; Renamed 'camera'
          {scene-id :db/id} :camera/scene ; Renamed 'scene'
          camera-shift :camera/point ; Renamed 'shift'
          camera-scale :camera/scale} :user/camera} user] ; Renamed 'scale'
    [{:db/id -1 ; New note entity
      :object/type :note/note
      :object/point
      (-> (vec/sub point (.-a screen-bounds)) ; Use screen-bounds
          (vec/div (or camera-scale 1))
          (vec/add (or camera-shift vec/zero)) ; Add or camera-shift
          (vec/shift -16) ; constant for note offset?
          (vec/rnd)) ; round to nearest integer coordinates
      :object/hidden true
      :object/locked true
      :note/label "Note"}
     {:db/id scene-id :scene/notes -1} ; Add ref to scene's notes
     {:db/id camera-id :camera/selected -1 :camera/draw-mode :select}]))

(defmethod event-tx-fn :note/change-icon
  [_db _event-type id icon-name]
  [[:db/add id :note/icon icon-name]])

(defmethod event-tx-fn :note/change-label
  [_db _event-type id value]
  [[:db/add id :note/label value]])

(defmethod event-tx-fn :note/change-description
  [_db _event-type id value]
  [[:db/add id :note/description value]])

(defmethod event-tx-fn :note/change-details
  [db _event-type id label desc] ; Changed 'data' to 'db'
  (let [user (ds/entity db [:db/ident :user])]
    [[:db/add id :note/label label]
     [:db/add id :note/description desc]
     [:db/retract (:db/id (:user/camera user)) :camera/selected id]]))

;; --- PLAYER SHEET EVENTS ---
(defmethod event-tx-fn :player-sheet/save
  [db {:keys [_id data] :as _event-payload}] ; 'id' is original editing-sheet-id (nil for new), 'data' is form map
  (let [;; The :id key within the 'data' map from the form is the definitive UUID for the sheet
        ;; (panel_player_sheets.cljs assigns one via random-uuid for new sheets before dispatch)
        sheet-uuid (:id data)
        _ (assert (uuid? sheet-uuid) (str "Player sheet data must have a valid UUID for its :id key. Got: " sheet-uuid))

        ;; Prepare the entity map for DataScript transaction.
        ;; It's assumed that keys in `data` from the form are already correctly namespaced
        ;; (e.g., :player-sheet/character-name) as defined in `initial-sheet-state`.
        ;; We remove the temporary client-side :id from the attributes to be saved,
        ;; as :player-sheet/id (based on sheet-uuid) is the actual unique identifying attribute.
        sheet-attributes-to-save (dissoc data :id)

        sheet-entity-map (assoc sheet-attributes-to-save
                                :db/id [:player-sheet/id sheet-uuid] ; Use lookup ref for :db/id
                                :player-sheet/id sheet-uuid          ; Ensure this key is definitely present and correct
                                :player-sheet/updated-at (js/Date.))

        ;; Check if an entity with this :player-sheet/id already exists in the db.
        is-new-sheet? (empty? (ds/q '[:find ?e .
                                      :in $ ?sheet-uuid
                                      :where [?e :player-sheet/id ?sheet-uuid]]
                                    db sheet-uuid))]

    (if is-new-sheet?
      ;; For a new sheet:
      (let [;; Ensure :player-sheet/created-at is set for new sheets
            created-entity-map (assoc sheet-entity-map
                                      :player-sheet/created-at (get sheet-entity-map :player-sheet/created-at (js/Date.)))]
        [created-entity-map ; Add/upsert the sheet entity itself
         ;; Link this new sheet to the :root entity's :root/player-sheets collection
         [:db/add [:db/ident :root] :root/player-sheets [:player-sheet/id sheet-uuid]]])
      ;; For an existing sheet:
      [sheet-entity-map]))) ; Just upsert the sheet entity data (identified by :db/id [:player-sheet/id sheet-uuid])