(ns ogres.app.component.panel
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.panel-data :as data]
            [ogres.app.component.panel-initiative :as initiative]
            [ogres.app.component.panel-lobby :as lobby]
            [ogres.app.component.panel-scene :as scene]
            [ogres.app.component.panel-tokens :as tokens]
            [ogres.app.component.panel-player-sheets :as player-sheets] ; 
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(def ^:private status-query
  [{:root/user [:user/type [:session/status :default :initial]]
    :root/session [:session/conns :session/room]}])

(def ^:private status-icon
  ($ icon {:name "globe-americas" :size 16}))

(defui status []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query status-query [:db/ident :root])
        {{type :user/type status :session/status} :root/user
         {code  :session/room
          conns :session/conns} :root/session} result
        connected (cond-> (count conns) (= type :host) (inc))]
    (case status
      :initial      ($ :button.button {:on-click #(dispatch :session/request)} status-icon "Start online game")
      :connecting   ($ :button.button {:disabled true} status-icon "Connecting...")
      :connected    ($ :button.button {:disabled true} status-icon "Connected / " code " / [ " connected " ]")
      :disconnected ($ :button.button {:disabled true} status-icon "Disconnected")
      ($ :button.button {:disabled true} status-icon "Status not known"))))

(def ^:private panel-data
  {:data       {:icon "wrench-adjustable-circle" :label "Manage local data"}
   :initiative {:icon "hourglass-split" :label "Initiative"}
   :lobby      {:icon "people-fill" :label "Online options"}
   :scene      {:icon "images" :label "Scene options"}
   :tokens     {:icon "person-circle" :label "Tokens"}})

(def ^:private panel-forms
  {:host [:tokens :scene :initiative :lobby :data :player-sheets] ; <<< ADDED :player-sheets
   :conn [:tokens :initiative :lobby :player-sheets]})              ; <<< ADDED :player-sheets

(def ^:private components
  {:data          {:form data/form}
   :initiative    {:form initiative/form :footer initiative/footer}
   :lobby         {:form lobby/form :footer lobby/footer}
   :scene         {:form scene/form}
   :tokens        {:form tokens/form :footer tokens/footer}
   :player-sheets {:form player-sheets/panel-root-component}}) ; <<< ADDED THIS ENTRY

(def ^:private query
  [[:user/type :default :conn]
   [:panel/selected :default :tokens] ; Default selected panel
   [:panel/expanded :default true]])

(defui container []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {type :user/type
         selected :panel/selected
         expanded :panel/expanded} result
        forms (get panel-forms type [])] ; Added get with default empty vector for safety
    ($ :.panel
      {:data-expanded expanded}
      (if expanded
        ($ :.panel-status
          ($ status)))
      ($ :ul.panel-tabs
        {:role "tablist"
         :aria-controls "form-panel"
         :aria-orientation "vertical"}
        ;; Ensure `panel-data` is correctly accessed for the available `forms`
        (for [key forms
              :let [panel-info (get panel-data key)
                    is-selected (= selected key)]
              :when panel-info] ; Only render if panel-info exists
          ($ :li.panel-tabs-tab
            {:key key :role "tab" :aria-selected (and expanded is-selected)}
            ($ :label {:aria-label (:label panel-info)}
              ($ :input
                {:type "radio"
                 :name "panel"
                 :value key
                 :checked (and expanded is-selected)
                 :on-change #(dispatch :user/select-panel key)})
              ($ icon {:name (:icon panel-info) :size 20}))))
        ($ :li.panel-tabs-control
          {:role "tab" :on-click #(dispatch :user/toggle-panel)}
          ($ :button {:type "button" :aria-label "Collapse or expand"}
            ($ icon {:name (if expanded "chevron-double-right" "chevron-double-left")}))))
      (if expanded
        ($ :.form
          {:id "form-panel"
           :role "tabpanel"
           :data-form (name selected)}
          ($ :.form-container
            ($ :.form-content
              (if-let [component-map (get components selected)]
                ($ :<>
                  (if-let [form-component (:form component-map)]
                    ($ :.form-body ($ form-component)))
                  (if-let [footer-component (:footer component-map)]
                    ($ :.form-footer ($ footer-component)))))
              ;; Added a fallback message if no component is found for a selected panel
              (when (nil? (get components selected))
                ($ :p "Selected panel content not found.")))))))))