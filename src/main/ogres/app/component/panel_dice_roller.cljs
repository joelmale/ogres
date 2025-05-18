;; src/main/ogres/app/component/panel_dice_roller.cljs
(ns ogres.app.component.panel-dice-roller
  (:require [uix.core :as uix :refer [defui $]]
            [ogres.app.component :refer [icon]] ; For the roll button icon
            [clojure.string :as str]))

;; Helper to parse integer from input, defaulting to a value if NaN or empty
(defn- parse-int [s default-val]
  (if (str/blank? s)
    default-val
    (let [n (js/parseInt s 10)]
      (if (js/isNaN n) default-val n))))

(def dice-types
  [{:value 4 :label "d4"}
   {:value 6 :label "d6"}
   {:value 8 :label "d8"}
   {:value 10 :label "d10"}
   {:value 12 :label "d12"}
   {:value 20 :label "d20"}
   {:value 100 :label "d100 (Percentile)"}])

(defui panel-content []
  (let [[num-dice set-num-dice!] (uix/use-state "1")
        [selected-die-type set-selected-die-type!] (uix/use-state 20) ; Default to d20
        [modifier-str set-modifier-str!] (uix/use-state "+0")
        [roll-results set-roll-results!] (uix/use-state nil) ; {:rolls [r1, r2..], :modifier mod, :total t}
        [roll-history set-roll-history!] (uix/use-state []) ; List of previous roll results strings

        handle-roll (fn []
                      (let [n-dice (parse-int num-dice 1)
                            die-type selected-die-type
                            mod-val (parse-int (str/replace modifier-str #"[^0-9\-]" "") 0) ; Extract number from modifier string
                            mod-sign (if (str/includes? modifier-str "-") -1 1)
                            actual-modifier (* mod-sign mod-val)

                            rolls (vec (for [_ (range n-dice)]
                                         (+ (rand-int die-type) 1)))
                            sum-of-rolls (apply + rolls)
                            total (+ sum-of-rolls actual-modifier)]
                        (let [result-map {:rolls rolls
                                          :sum sum-of-rolls
                                          :modifier actual-modifier
                                          :total total}
                              result-str (str n-dice "d" die-type
                                              (when (not= actual-modifier 0)
                                                (if (pos? actual-modifier) "+" "") actual-modifier)
                                              ": " (str/join ", " rolls)
                                              (when (not= actual-modifier 0) (str " (mod: " actual-modifier ")"))
                                              " = " total)]
                          (set-roll-results! result-map)
                          (set-roll-history! (fn [prev-history]
                                               (vec (take 5 (cons result-str prev-history))))))))] ; Keep last 5 rolls

    ($ :form.form-dice-roller {:on-submit (fn [e] (.preventDefault e) (handle-roll))}
      ($ :header ($ :h2 "Dice Roller"))

      ($ :fieldset.fieldset
        ($ :legend "Roll Configuration")
        ($ :div.dice-inputs ; Class for styling inputs in a row or grid
          ($ :div.form-group
            ($ :label {:htmlFor :num-dice} "Number of Dice")
            ($ :input.text.text-ghost
              {:id :num-dice :type "number" :min "1" :value num-dice
               :on-change #(set-num-dice! (.-target.value %))}))

          ($ :div.form-group
            ($ :label {:htmlFor :dice-type} "Die Type")
            ($ :select.select-ghost ; Assuming .select-ghost is a styled select
              {:id :dice-type :value selected-die-type
               :on-change #(set-selected-die-type! (js/parseInt (.-target.value %) 10))}
              (for [die dice-types]
                ($ :option {:key (:value die) :value (:value die)} (:label die)))))

          ($ :div.form-group
            ($ :label {:htmlFor :modifier} "Modifier")
            ($ :input.text.text-ghost
              {:id :modifier :type "text" :value modifier-str
               :placeholder "+0"
               :on-change #(set-modifier-str! (.-target.value %))}))))

      ($ :div.form-actions
        ($ :button.button.button-primary {:type "submit"}
          ($ icon {:name "play-fill"}) ; Or use your "dice-5" icon if you prefer
          "Roll Dice"))

      (when roll-results
        ($ :fieldset.fieldset.results-area {:key (gensym "roll-result-")}; Use gensym for a unique key to help React re-render
          ($ :legend "Last Roll Result")
          ($ :p.roll-detail "Dice: " (str/join ", " (:rolls roll-results)))
          (when (not= 0 (:modifier roll-results))
            ($ :p.roll-modifier "Modifier: " (if (pos? (:modifier roll-results)) "+") (:modifier roll-results)))
          ($ :p.roll-total ($ :strong "Total: " (:total roll-results)))))

      (when (seq roll-history)
        ($ :fieldset.fieldset.history-area
          ($ :legend "Recent Rolls")
          ($ :ul.roll-history-list
            (for [[idx roll-str] (map-indexed vector roll-history)]
              ($ :li {:key idx} roll-str))))))))