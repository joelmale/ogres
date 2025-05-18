(ns ogres.app.component
  (:require [ogres.app.const :refer [PATH]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]))

(defn ^:private create-range
  "Generates a vector of page numbers and spacing sentinels for pagination.
   Parameters:
     - min: The first page number (integer).
     - max: The last page number (integer).
     - val: The currently selected page number (integer).
   Returns a vector containing page numbers and/or sentinels (`:spacel`, `:spacer`).
   The vector will have between 1 and 7 elements.
   ```
   (create-range 1 78 23) ;; [1 :spacel 22 23 24 :spacer 78]
   ```"
  [min max val]
  (cond (< (- max min) 7)
        (range min (inc max))
        (<= val (+ min 3))
        [min (+ min 1) (+ min 2) (+ min 3) (+ min 4) :spacel max]
        (>= val (- max 3))
        [min :spacel (- max 4) (- max 3) (- max 2) (- max 1) max]
        :else
        [min :spacel (- val 1) val (+ val 1) :spacer max]))

(defui icon
  "Renders the `<svg>` definition found in `icons.svg` matching the given name.
   ```
   ($ icon {:name 'arrow-right-short' :size 16})
   ```"
  [{:keys [name size] :or {size 22}}]
  ($ :svg
    {:fill "currentColor"
     :role "presentation"
     :class "icon"
     :width size
     :height size}
    ($ :use {:href (str PATH "/icons.svg" "#icon-" name)})))

(defui pagination
  "Renders a `<nav>` element that provides a means to navigate through
   a paginated resource, including buttons to go backward, forward,
   and directly to different pages.
   ```
   ($ pagination
     {:name 'token-gallery'
      :pages 3
      :value 1
      :on-change (fn [page] ...)})
   ```"
  [{:keys [class-name name label pages value on-change]
    :or   {pages 10 value 1 on-change identity}}]
  ($ :nav {:role "navigation" :aria-label label}
    ($ :ol.pagination
      {:class (if class-name (str "pagination-" class-name))}
      ($ :li
        ($ :button
          {:aria-disabled (= value 1)
           :aria-label "Previous page"
           :on-click
           (fn []
             (if (not (= value 1))
               (on-change (dec value))))}
          ($ icon {:name "chevron-left" :size 16})))
      (for [term (create-range 1 pages value)]
        ($ :li {:key term}
          (if (or (= term :spacel) (= term :spacer))
            ($ :label {:role "presentation"} \…)
            ($ :label {:aria-label value :aria-current (= term value)}
              ($ :input
                {:type "radio"
                 :name name
                 :value value
                 :checked (= term value)
                 :on-change #(on-change term)}) term))))
      ($ :li
        ($ :button
          {:aria-disabled (= value pages)
           :aria-label "Next page"
           :on-click
           (fn []
             (if (not (= value pages))
               (on-change (inc value))))}
          ($ icon {:name "chevron-right" :size 16}))))))

(defui image
  "Renders the image identified by the given hash. Accepts a
   render function as its children which is passed a URL that
   references the image resource.
   ```
   ($ image {:hash 'fa7b887b1ce364732beb9ac70892775a'}
     (fn [url]
       ($ :img {:src url})))
   ```"
  [{:keys [hash children]}]
  (let [url (hooks/use-image hash)]
    (children url)))

(defui form-field ;; A reusable form field component
  "Renders a form field with a label, input, and optional extra properties.
   ```
   ($ form-field
     {:label 'Name'
      :id 'name'
      :type 'text'
      :value 'John Doe'
      :placeholder 'Enter your name'
      :on-change (fn [e] (println (.-target.value e)))})
   ```"
  [{:keys [label id type value placeholder on-change extra-props]
                    :or {type "text" extra-props {}}}]
  ($ :div.form-group ; You'll need a CSS class for .form-group
    (when label ($ :label {:htmlFor id} label))
    ($ :input.text.text-ghost ; Assuming .text.text-ghost are existing global styles
      (merge
       {:id id
        :name id
        :type type
        :value (if (nil? value) "" value) ; Ensure value is not nil for controlled input
        :placeholder placeholder
        :on-change on-change}
       extra-props))))

(defui textarea-field
  "A reusable component for a label and a textarea
   ```
   ($ textarea-field
     {:label 'Description'
      :id 'description'
      :value 'This is a description.'
      :placeholder 'Enter description...'
      :on-change (fn [e] (println (.-target.value e)))})
   ```"
  [{:keys [label id value placeholder on-change rows extra-props]
    :or   {rows 3 extra-props {}}}]
  ($ :div.form-group
    (when label ($ :label {:htmlFor id} label))
    ($ :textarea.text.text-ghost
      (merge
       {:id id
        :name (name id)
        :rows rows
        :value (if (nil? value) "" value)
        :placeholder placeholder
        :on-change on-change}
       extra-props))))

(defui checkbox-field
  "A reusable component for a label, an icon, and a checkbox.
   ```
   ($ checkbox-field {:id \"agree-terms\" :label \"I agree to the terms\"
                      :checked @agreed? :on-change #(reset! agreed? (.-target.checked %))})
   ```"
  [{:keys [label id checked on-change extra-props]
    :or {extra-props {}}}]
  ($ :label.checkbox
    ($ :input
      (merge
       {:type "checkbox"
        :id id
        :name (name id)
        :checked (boolean checked) ; Ensure it's a boolean
        :on-change on-change}
       extra-props))
    ($ icon {:name (if checked "check" "square") :size 16}) ; Uses your existing icon component
    (when label ($ :span.checkbox-label " " label)))) ; Class for styling the text part of label

(defui fullscreen-dialog
  "Renders children in a fullscreen dialog, closable via Escape or backdrop click.
   ```
   ($ fullscreen-dialog {:on-close #(js/alert \"Dialog closing!\")}
     ($ :div {:style {:padding \"20px\" :background \"white\"}}
       ($ :h2 \"My Dialog Content\")))
   ```"
  [props]
  (let [element (.querySelector js/document "#root")
        dialog  (uix/use-ref nil)]
    (hooks/use-shortcut ["Escape"]
      (:on-close props))
    (dom/create-portal
     ($ :.fullscreen-dialog
       {:ref dialog
        :role "dialog"
        :tab-index -1
        :on-click
        (fn [event]
          (if (= (.-target event) (deref dialog))
            ((:on-close props) event)))}
       ($ :.fullscreen-dialog-body {:role "document"}
         ($ :.fullscreen-dialog-content
           (:children props)))) element)))
