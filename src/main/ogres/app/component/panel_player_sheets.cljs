;; src/main/ogres/app/component/panel_player_sheets.cljs
(ns ogres.app.component.panel-player-sheets
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]

;; --- Helper: Form Field Components ---
(defui form-field [{:keys [label id type value placeholder on-change extra-props]
                    :or {type "text" extra-props {}}}]
  ($ :div.form-group
    (when label ($ :label {:htmlFor id} label))
    ($ :input.text.text-ghost
      (merge
       {:id id
        :name id
        :type type
        :value (if (nil? value) "" value)
        :placeholder placeholder
        :on-change on-change}
       extra-props))))

(defui textarea-field [{:keys [label id value placeholder on-change rows]
                        :or {rows 3}}]
  ($ :div.form-group
    (when label ($ :label {:htmlFor id} label))
    ($ :textarea.text.text-ghost
      {:id id
       :name id
       :rows rows
       :value (if (nil? value) "" value)
       :placeholder placeholder
       :on-change on-change})))

(defui checkbox-field [{:keys [label id checked on-change]}]
  ($ :label.checkbox
    ($ :input {:type "checkbox" :id id :name id :checked (boolean checked) :on-change on-change})
    ($ icon {:name (if checked "check" "square") :size 16})
    (when label ($ :span.checkbox-label " " label))))

;; --- Initial Empty State for a New Character Sheet ---
(def initial-sheet-state
  {:id nil
   :character-name "" :class-level "" :background "" :player-name "" :race "" :alignment "" :experience-points 0
   :strength 10 :dexterity 10 :constitution 10 :intelligence 10 :wisdom 10 :charisma 10
   :inspiration false :proficiency-bonus 2
   :st-strength-prof false :st-strength-bonus 0 :st-dexterity-prof false :st-dexterity-bonus 0
   :st-constitution-prof false :st-constitution-bonus 0 :st-intelligence-prof false :st-intelligence-bonus 0
   :st-wisdom-prof false :st-wisdom-bonus 0 :st-charisma-prof false :st-charisma-bonus 0
   :skill-acrobatics-prof false :skill-acrobatics-bonus 0 :skill-animal-handling-prof false :skill-animal-handling-bonus 0
   :skill-arcana-prof false :skill-arcana-bonus 0 :skill-athletics-prof false :skill-athletics-bonus 0
   :skill-deception-prof false :skill-deception-bonus 0 :skill-history-prof false :skill-history-bonus 0
   :skill-insight-prof false :skill-insight-bonus 0 :skill-intimidation-prof false :skill-intimidation-bonus 0
   :skill-investigation-prof false :skill-investigation-bonus 0 :skill-medicine-prof false :skill-medicine-bonus 0
   :skill-nature-prof false :skill-nature-bonus 0 :skill-perception-prof false :skill-perception-bonus 0
   :skill-performance-prof false :skill-performance-bonus 0 :skill-persuasion-prof false :skill-persuasion-bonus 0
   :skill-religion-prof false :skill-religion-bonus 0 :skill-sleight-of-hand-prof false :skill-sleight-of-hand-bonus 0
   :skill-stealth-prof false :skill-stealth-bonus 0 :skill-survival-prof false :skill-survival-bonus 0
   :passive-perception 10 :armor-class 10 :initiative 0 :speed "30ft"
   :hp-max 10 :hp-current 10 :hp-temp 0 :hit-dice-total "1d8" :hit-dice-current "1d8"
   :death-saves-successes 0 :death-saves-failures 0
   :attacks-spellcasting "" :personality-traits "" :ideals "" :bonds "" :flaws ""
   :features-traits "" :proficiencies-languages "" :equipment ""
   :backstory "" :appearance "" :allies-organizations "" :treasure ""})

;; --- D&D 5e Data ---
(def attributes-list [:strength :dexterity :constitution :intelligence :wisdom :charisma])
(def skills-list
  [{:key :skill-acrobatics :label "Acrobatics" :attr :dexterity} {:key :skill-animal-handling :label "Animal Handling" :attr :wisdom}
   {:key :skill-arcana :label "Arcana" :attr :intelligence} {:key :skill-athletics :label "Athletics" :attr :strength}
   {:key :skill-deception :label "Deception" :attr :charisma} {:key :skill-history :label "History" :attr :intelligence}
   {:key :skill-insight :label "Insight" :attr :wisdom} {:key :skill-intimidation :label "Intimidation" :attr :charisma}
   {:key :skill-investigation :label "Investigation" :attr :intelligence} {:key :skill-medicine :label "Medicine" :attr :wisdom}
   {:key :skill-nature :label "Nature" :attr :intelligence} {:key :skill-perception :label "Perception" :attr :wisdom}
   {:key :skill-performance :label "Performance" :attr :charisma} {:key :skill-persuasion :label "Persuasion" :attr :charisma}
   {:key :skill-religion :label "Religion" :attr :intelligence} {:key :skill-sleight-of-hand :label "Sleight of Hand" :attr :dexterity}
   {:key :skill-stealth :label "Stealth" :attr :dexterity} {:key :skill-survival :label "Survival" :attr :wisdom}])

;; --- Character Sheet Form Component ---
(defui CharacterSheetForm [{:keys [current-sheet-prop on-sheet-change-prop on-save on-cancel on-generate-ai]}]
  (let [handle-change-factory (fn [field-key type]
                                (fn [event]
                                  (let [target (.-target event)
                                        value (if (= (.-type target) "checkbox")
                                                (.-checked target)
                                                (.-value target))
                                        parsed-value (case type
                                                       :number (let [num-str (str value) ; Ensure it's a string before parsing
                                                                     num (js/parseInt num-str 10)]
                                                                 (if (js/isNaN num) (if (= num-str "") "" 0) num)) ; Allow empty, default to 0 if unparseable
                                                       :boolean (boolean value)
                                                       value)] ; Default to string
                                    (on-sheet-change-prop (assoc current-sheet-prop field-key parsed-value)))))]

    ($ :form.form-player-sheets {:on-submit (fn [event] (.preventDefault event) (on-save))}
      ($ :header ($ :h2 (if (:id current-sheet-prop) "Edit Player Sheet" "Create New Player Sheet")))

      ($ :fieldset.fieldset.character-basics-grid
        ($ :legend "Character Basics")
        (form-field {:label "Character Name" :id :character-name :value (:character-name current-sheet-prop)
                     :on-change (handle-change-factory :character-name :string)})
        (form-field {:label "Class & Level" :id :class-level :value (:class-level current-sheet-prop)
                     :placeholder "e.g., Fighter 5" :on-change (handle-change-factory :class-level :string)})
        (form-field {:label "Background" :id :background :value (:background current-sheet-prop)
                     :on-change (handle-change-factory :background :string)})
        (form-field {:label "Player Name" :id :player-name :value (:player-name current-sheet-prop)
                     :on-change (handle-change-factory :player-name :string)})
        (form-field {:label "Race" :id :race :value (:race current-sheet-prop)
                     :on-change (handle-change-factory :race :string)})
        (form-field {:label "Alignment" :id :alignment :value (:alignment current-sheet-prop)
                     :on-change (handle-change-factory :alignment :string)})
        (form-field {:label "Experience" :id :experience-points :type "number"
                     :value (:experience-points current-sheet-prop) :on-change (handle-change-factory :experience-points :number)}))

      ($ :fieldset.fieldset
        ($ :legend "Attributes")
        ($ :div.attribute-grid
          (for [attr attributes-list]
            ($ :div.attribute-item {:key (name attr)}
              (form-field {:label (clojure.string/capitalize (name attr)) :id attr :type "number"
                           :value (get current-sheet-prop attr) :on-change (handle-change-factory attr :number)
                           :extra-props {:min 1 :max 30}})))))

      ($ :fieldset.fieldset
        ($ :legend "Vitals & Combat")
        ($ :div.combat-grid
          (form-field {:label "Inspiration" :id :inspiration :type "text" :value (:inspiration current-sheet-prop) :on-change (handle-change-factory :inspiration :string)})
          (form-field {:label "Proficiency Bonus" :id :proficiency-bonus :type "number" :value (:proficiency-bonus current-sheet-prop) :on-change (handle-change-factory :proficiency-bonus :number)})
          (form-field {:label "Armor Class" :id :armor-class :type "number" :value (:armor-class current-sheet-prop) :on-change (handle-change-factory :armor-class :number)})
          (form-field {:label "Initiative" :id :initiative :type "number" :value (:initiative current-sheet-prop) :on-change (handle-change-factory :initiative :number)})
          (form-field {:label "Speed" :id :speed :value (:speed current-sheet-prop) :placeholder "e.g., 30ft" :on-change (handle-change-factory :speed :string)})
          (form-field {:label "Max HP" :id :hp-max :type "number" :value (:hp-max current-sheet-prop) :on-change (handle-change-factory :hp-max :number)})
          (form-field {:label "Current HP" :id :hp-current :type "number" :value (:hp-current current-sheet-prop) :on-change (handle-change-factory :hp-current :number)})
          (form-field {:label "Temp HP" :id :hp-temp :type "number" :value (:hp-temp current-sheet-prop) :on-change (handle-change-factory :hp-temp :number)})
          (form-field {:label "Total Hit Dice" :id :hit-dice-total :value (:hit-dice-total current-sheet-prop) :placeholder "e.g., 5d10" :on-change (handle-change-factory :hit-dice-total :string)})
          (form-field {:label "Current Hit Dice" :id :hit-dice-current :value (:hit-dice-current current-sheet-prop) :placeholder "e.g., 5d10" :on-change (handle-change-factory :hit-dice-current :string)})
          ;; TODO: Death Saves UI (e.g., 3 checkboxes for success, 3 for failure for :death-saves-successes :death-saves-failures)
          ))

      ($ :fieldset.fieldset.saving-throws-skills-grid
        ($ :legend "Saving Throws")
        ($ :div.saving-throws-list
          (for [attr attributes-list]
            ($ :div.saving-throw-item {:key (str "st-" (name attr))}
              (checkbox-field {:label (clojure.string/capitalize (name attr))
                               :id (keyword (str "st-" (name attr) "-prof"))
                               :checked (get current-sheet-prop (keyword (str "st-" (name attr) "-prof")))
                               :on-change (handle-change-factory (keyword (str "st-" (name attr) "-prof")) :boolean)})
              (form-field {:id (keyword (str "st-" (name attr) "-bonus")) :type "number" :placeholder "Bonus"
                           :value (get current-sheet-prop (keyword (str "st-" (name attr) "-bonus")))
                           :on-change (handle-change-factory (keyword (str "st-" (name attr) "-bonus")) :number)
                           :extra-props {:style {:max-width "60px"}}})))))

      ($ :fieldset.fieldset.saving-throws-skills-grid
        ($ :legend "Skills")
        ($ :div.skills-list
          (for [{skill-key :key skill-label :label skill-attr :attr} skills-list]
            ($ :div.skill-item {:key (name skill-key)}
              (checkbox-field {:label (str skill-label " (" (subs (clojure.string/upper-case (name skill-attr)) 0 3) ")")
                               :id (keyword (str (name skill-key) "-prof"))
                               :checked (get current-sheet-prop (keyword (str (name skill-key) "-prof")))
                               :on-change (handle-change-factory (keyword (str (name skill-key) "-prof")) :boolean)})
              (form-field {:id (keyword (str (name skill-key) "-bonus")) :type "number" :placeholder "Bonus"
                           :value (get current-sheet-prop (keyword (str (name skill-key) "-bonus")))
                           :on-change (handle-change-factory (keyword (str (name skill-key) "-bonus")) :number)
                           :extra-props {:style {:max-width "60px"}}})))))

      ($ :fieldset.fieldset
        ($ :legend "Passive Wisdom (Perception)")
        (form-field {:label nil :id :passive-perception :type "number" :value (:passive-perception current-sheet-prop)
                     :on-change (handle-change-factory :passive-perception :number)}))

      ($ :fieldset.fieldset
        ($ :legend "Attacks & Spellcasting")
        (textarea-field {:id :attacks-spellcasting :rows 6 :value (:attacks-spellcasting current-sheet-prop)
                         :placeholder "List attacks, spells, spell slots, DC, attack bonus..."
                         :on-change (handle-change-factory :attacks-spellcasting :string)}))

      ($ :fieldset.fieldset
        ($ :legend "Personality")
        (textarea-field {:label "Personality Traits" :id :personality-traits :value (:personality-traits current-sheet-prop)
                         :on-change (handle-change-factory :personality-traits :string)})
        (textarea-field {:label "Ideals" :id :ideals :value (:ideals current-sheet-prop)
                         :on-change (handle-change-factory :ideals :string)})
        (textarea-field {:label "Bonds" :id :bonds :value (:bonds current-sheet-prop)
                         :on-change (handle-change-factory :bonds :string)})
        (textarea-field {:label "Flaws" :id :flaws :value (:flaws current-sheet-prop)
                         :on-change (handle-change-factory :flaws :string)}))

      ($ :fieldset.fieldset
        ($ :legend "Features & Traits")
        (textarea-field {:id :features-traits :rows 8 :value (:features-traits current-sheet-prop)
                         :on-change (handle-change-factory :features-traits :string)}))

      ($ :fieldset.fieldset
        ($ :legend "Other Proficiencies & Languages")
        (textarea-field {:id :proficiencies-languages :rows 4 :value (:proficiencies-languages current-sheet-prop)
                         :on-change (handle-change-factory :proficiencies-languages :string)}))

      ($ :fieldset.fieldset
        ($ :legend "Equipment")
        (textarea-field {:id :equipment :rows 8 :value (:equipment current-sheet-prop)
                         :on-change (handle-change-factory :equipment :string)}))

      ($ :fieldset.fieldset
        ($ :legend "Character Details")
        (textarea-field {:label "Appearance" :id :appearance :rows 3 :value (:appearance current-sheet-prop)
                         :on-change (handle-change-factory :appearance :string)})
        (textarea-field {:label "Allies & Organizations" :id :allies-organizations :rows 3 :value (:allies-organizations current-sheet-prop)
                         :on-change (handle-change-factory :allies-organizations :string)})
        (textarea-field {:label "Backstory" :id :backstory :rows 5 :value (:backstory current-sheet-prop)
                         :on-change (handle-change-factory :backstory :string)})
        (textarea-field {:label "Treasure" :id :treasure :rows 3 :value (:treasure current-sheet-prop)
                         :on-change (handle-change-factory :treasure :string)}))

      ($ :div.form-actions
        ($ :button.button.button-primary {:type "submit"}
          ($ icon {:name "save"}) " Save Sheet")
        (when on-generate-ai
          ($ :button.button.button-neutral {:type "button" :on-click on-generate-ai :style {:margin-left "10px"}}
            ($ icon {:name "gem"}) " Suggest with AI"))
        ($ :button.button.button-neutral {:type "button" :on-click on-cancel :style {:margin-left "auto"}}
          "Cancel")))))

;; --- List View for Player Sheets ---
(defui SheetListView [{:keys [sheets on-select-sheet on-create-new on-generate-ai-new]}]
  ($ :div.player-sheets-list-view
    ($ :header.list-header
      ($ :h2 "Player Sheets")
      ($ :div.list-actions
        ($ :button.button.button-primary {:on-click on-create-new}
          ($ icon {:name "plus"}) " Create New Sheet")
        ($ :button.button.button-neutral {:on-click on-generate-ai-new :style {:margin-left "10px"}}
          ($ icon {:name "gem"}) " Generate New with AI")))
    (if (empty? sheets)
      ($ :p.empty-list-message "No player sheets. Create one or generate with AI!")
      ($ :ul.sheets-list
        (for [sheet sheets] ; Assuming sheets have :id, :character-name, :class-level
          ($ :li.sheet-item {:key (:id sheet) :tabIndex 0
                             :on-click #(on-select-sheet (:id sheet))
                             :on-key-down (fn [e] (when (or (= (.-key e) "Enter") (= (.-key e) " ")) (.preventDefault e) (on-select-sheet (:id sheet))))}
            ($ :div.sheet-summary-info
              ($ :strong.sheet-name (:character-name sheet))
              ($ :span.sheet-class-level (when (:class-level sheet) (str " - " (:class-level sheet)))))))))))

;; --- Main Panel Component (this is what panel.cljs will use) ---
(defui panel-root-component []
  (let [dispatch (hooks/use-dispatch)
        [view-mode set-view-mode!] (uix/use-state :list) ; :list or :form
        [all-sheets set-all-sheets!] (uix/use-state [])  ; List of sheet summaries {id, name, class}

        ;; State for the sheet currently being created or edited in the form
        [current-sheet-data set-current-sheet-data!] (uix/use-state initial-sheet-state)
        [editing-sheet-id set-editing-sheet-id!] (uix/use-state nil) ; UUID of sheet being edited, or nil for new

        ;; Placeholder for fetching all sheet summaries from backend via DataScript
        ;; This query should match the attributes you want for the list view.
        ;; And it should be namespaced according to your schema (e.g., :player-sheet/id)
        fetched-data (hooks/use-query [{:root/player-sheets [:player-sheet/id :player-sheet/character-name :player-sheet/class-level]}] [:db/ident :root])]

    (uix/use-effect
     (fn []
       (if-let [sheets-from-db (:root/player-sheets fetched-data)]
         (do
           (js/console.log "Fetched sheets for list:" sheets-from-db)
           (set-all-sheets! sheets-from-db))
         (do
           (js/console.log "No sheets found in fetched-data, or fetched-data is nil. Setting all-sheets to empty.")
           (set-all-sheets! []))) ; Ensure it's an empty vector if no data
       #js {:cleanup #()}) ; No-op cleanup
     [fetched-data]) ; Re-run when fetched-data changes

    (letfn [(handle-create-new []
              (set-editing-sheet-id! nil)
              (set-current-sheet-data! (assoc initial-sheet-state :id (random-uuid))) ; Assign temp client ID
              (set-view-mode! :form))

            (handle-generate-ai-new []
              (js/console.log "Top-level Generate AI clicked")
              (dispatch :player-sheet/gemini-generate-for-new {:preferences {}})
              ;; The actual data setting (set-current-sheet-data!) should happen
              ;; in response to an event triggered by the backend after AI generation.
              ;; For now, we optimistically switch and show a placeholder.
              (set-editing-sheet-id! nil)
              (set-current-sheet-data! (assoc initial-sheet-state :id (random-uuid) :character-name "AI Character (Generating...)"))
              (set-view-mode! :form))

            (handle-select-sheet [sheet-id]
              (js/console.log "Selected sheet ID for editing:" sheet-id)
              ;; TODO: Dispatch to fetch FULL sheet data for sheet-id if `all-sheets` only contains summaries.
              ;; (dispatch :player-sheet/fetch-full {:id sheet-id})
              ;; The response to that dispatch would then call set-current-sheet-data!
              (if-let [selected-full-sheet (first (filter #(= (:player-sheet/id %) sheet-id) all-sheets))] ; Assuming full data in all-sheets for now
                (do
                  (set-editing-sheet-id! sheet-id)
                  (set-current-sheet-data! (merge initial-sheet-state selected-full-sheet)) ; Merge to ensure all form fields are present
                  (set-view-mode! :form))
                (do
                  (js/console.error "Sheet with ID" sheet-id "not found in summary list. Fetching may be needed." all-sheets)
                  ;; Fallback: go to blank form if sheet not found in current list (shouldn't happen if list is accurate)
                  (handle-create-new))))

            (handle-form-save []
              (js/console.log "Saving sheet data:" current-sheet-data "for ID:" editing-sheet-id)
              ;; If editing-sheet-id is nil, it implies a new sheet.
              ;; The :id within current-sheet-data should be the new UUID assigned in handle-create-new or handle-generate-ai-new
              (dispatch :player-sheet/save {:id (or editing-sheet-id (:id current-sheet-data))
                                            :data current-sheet-data})
              (set-editing-sheet-id! nil)
              (set-current-sheet-data! initial-sheet-state)
              (set-view-mode! :list)
              ;; TODO: Ideally, :player-sheet/save handler or subsequent event might trigger a refresh of all-sheets
              ;; For now, user goes back to list, data will refresh on next query update.
              )

            (handle-form-cancel []
              (set-editing-sheet-id! nil)
              (set-current-sheet-data! initial-sheet-state)
              (set-view-mode! :list))

            (handle-form-generate-ai []
              (js/console.log "In-form Generate AI clicked for sheet:" current-sheet-data)
              (dispatch :player-sheet/gemini-generate-for-form {:sheet-context current-sheet-data})
              ;; The response from this dispatch should update current-sheet-data
              ;; For demo:
              (set-current-sheet-data! (update current-sheet-data :character-name #(str % " + AI Sparkle!"))))]

      ($ :div.player-sheets-panel-container
        (case view-mode
          :list ($ SheetListView {:sheets all-sheets
                                  :on-select-sheet handle-select-sheet
                                  :on-create-new handle-create-new
                                  :on-generate-ai-new handle-generate-ai-new})
          :form ($ CharacterSheetForm {:current-sheet-prop current-sheet-data
                                       :on-sheet-change-prop set-current-sheet-data!
                                       :on-save handle-form-save
                                       :on-cancel handle-form-cancel
                                       :on-generate-ai handle-form-generate-ai})
          ($ :p "Loading player sheets...")))))) ; Default/fallback case