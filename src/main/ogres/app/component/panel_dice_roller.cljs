(ns ogres.app.component.panel-dice-roller
  (:require [uix.core :as uix :refer [defui $]]
            [ogres.app.component :refer [icon]]
            [clojure.string :as str]))

(def dice-types-config
  [{:sides 4 :label "D4"} {:sides 6 :label "D6"} {:sides 8 :label "D8"}
   {:sides 10 :label "D10"} {:sides 12 :label "D12"} {:sides 20 :label "D20"}
   {:sides 100 :label "D100"}])

(defn- safe-parse-int [s default-value]
  (if (str/blank? (str s))
    default-value
    (let [n (js/parseInt (str s) 10)]
      (if (js/isNaN n) default-value n))))

(defui panel-content []
  (let [[dice-quantities set-dice-quantities!] (uix/use-state
                                                (into {} (for [d dice-types-config] [(:sides d) 0])))
        [modifier set-modifier!] (uix/use-state 0)
        [last-roll-details set-last-roll-details!] (uix/use-state nil)
        [roll-history set-roll-history!] (uix/use-state [])
        handle-quantity-change (fn [sides event]
                                 (let [qty-str (.. event -target -value)
                                       qty (safe-parse-int qty-str 0)]
                                   (set-dice-quantities! (fn [prev] (assoc prev sides (max 0 qty))))))
        handle-modifier-change (fn [delta]
                                 (set-modifier! (fn [curr] (+ curr delta))))
        handle-roll (fn []
                      (let [rolls-by-type (transient [])
                            total-sum (transient [])
                            expr-parts (transient [])
                            mod modifier]
                        (doseq [die-config dice-types-config]
                          (let [sides (:sides die-config)
                                qty (get dice-quantities sides 0)]
                            (when (pos? qty)
                              (conj! expr-parts (str qty "d" sides))
                              (let [rolls (vec (for [_ (range qty)] (+ 1 (rand-int sides))))]
                                (conj! rolls-by-type {:type (str "D" sides)
                                                      :count qty
                                                      :rolls rolls})
                                (doseq [r rolls] (conj! total-sum r))))))
                        (let [rolls (persistent! rolls-by-type)
                              sum (apply + (persistent! total-sum))
                              expr (str/join " + " (persistent! expr-parts))]
                          (if (empty? rolls)
                            (set-last-roll-details! {:error "Please enter a quantity for at least one die type."})
                            (let [grand-total (+ sum mod)
                                  summary (str expr
                                               (when (not= mod 0)
                                                 (str (if (pos? mod) " + " " - ") (Math/abs mod))))
                                  full-result (str summary " = " grand-total
                                                   " (Rolls: "
                                                   (str/join "; "
                                                             (map #(str (:count %) (:type %) ": ["
                                                                        (str/join ", " (:rolls %)) "]")
                                                                  rolls))
                                                   (when (not= mod 0)
                                                     (str ", Mod: " (if (pos? mod) "+") mod))
                                                   ")")]
                              (set-last-roll-details! {:summary summary
                                                       :details rolls
                                                       :sum sum
                                                       :mod mod
                                                       :total grand-total})
                              (set-roll-history! (fn [prev] (vec (take 5 (cons full-result prev))))))))))]
    ($ :div.dice-roller-panel-content
      ;; === Main header ===
      ($ :header ($ :h2 "Dice Roller"))

      ;; === Dice selection fieldset ===
      ($ :fieldset.fieldset.roll-configuration-fieldset
        ($ :legend "Dice & Quantities")
        ($ :div.dice-entry-list
          (for [die-config dice-types-config
                :let [sides (:sides die-config)
                      label (:label die-config)]]
            ($ :div.die-row {:key sides}
              ($ :label {:htmlFor (str "qty-d" sides)}
                label)
              ($ icon {:name "dice-5" :size 16})
              ($ :input.text.text-ghost.quantity-input
                {:type "number"
                 :id (str "qty-d" sides)
                 :min "0"
                 :value (get dice-quantities sides 0)
                 :on-change #(handle-quantity-change sides %)})))))

      ;; === Modifier controls ===
      ($ :fieldset.fieldset.modifier-fieldset
        ($ :legend "Global Modifier")
        ($ :div.modifier-controls
          ($ :button.button.button-neutral {:type "button"
                                            :aria-label "Decrement modifier"
                                            :on-click #(handle-modifier-change -1)} "-")
          ($ :span.modifier-value-display {} (if (pos? modifier) (str "+" modifier) modifier))
          ($ :button.button.button-neutral {:type "button"
                                            :aria-label "Increment modifier"
                                            :on-click #(handle-modifier-change 1)} "+")))

      ;; === Roll button ===
      ($ :div.form-actions
        ($ :button.button.button-primary.roll-all-button {:type "button" :on-click handle-roll}
          ($ icon {:name "play-fill"})
          "Roll Dice"))

      ;; === Last Roll Result ===
      (when last-roll-details
        ($ :fieldset.fieldset.results-area {:key (gensym "roll-result-")}
          ($ :legend "Last Roll Result")
          (if (:error last-roll-details)
            ($ :p.roll-error (:error last-roll-details))
            ($ :div
              ($ :p.roll-summary-string (:summary last-roll-details))
              (for [die-group (:details last-roll-details)]
                ($ :p.roll-detail {:key (:type die-group)}
                  (str (:count die-group) (:type die-group) ": ["
                       (str/join ", " (:rolls die-group)) "]")))
              (when (not= 0 (:mod last-roll-details))
                ($ :p.roll-modifier "Modifier: " (if (pos? (:mod last-roll-details)) "+") (:mod last-roll-details)))
              ($ :p.total-display "Total: " (:total last-roll-details))))))

      ;; === Roll History ===
      (when (seq roll-history)
        ($ :fieldset.fieldset.history-area
          ($ :legend "Recent Rolls")
          ($ :ul.roll-history-list
            (for [[idx roll-str] (map-indexed vector roll-history)]
              ($ :li {:key idx} roll-str))))))))