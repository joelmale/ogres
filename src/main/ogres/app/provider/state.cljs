(ns ogres.app.provider.state
  (:require [cognitect.transit :as t]
            [datascript.core :as ds]
            [goog.functions :refer [throttle]]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.provider.events :as events]
            [ogres.app.serialize :refer [reader writer]]
            [ogres.app.provider.idb :as idb]
            [ogres.app.vec :as vec] ; Assuming vec/zero is defined here
            [uix.core :as uix :refer [defui $]]))

(def schema
  {:camera/scene      {:db/valueType :db.type/ref}
   :camera/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :db/ident          {:db/unique :db.unique/identity}
   :image/hash        {:db/unique :db.unique/identity}
   :image/thumbnail   {:db/valueType :db.type/ref :db/isComponent true}
   :initiative/played {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :initiative/turn   {:db/valueType :db.type/ref}
   :root/scene-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/session      {:db/valueType :db.type/ref :db/isComponent true}
   :root/token-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/user         {:db/valueType :db.type/ref :db/isComponent true}
   :scene/image       {:db/valueType :db.type/ref}
   :scene/initiative  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/masks       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/shapes      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/tokens      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/notes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :session/conns     {:db/valueType :db.type/ref :db.cardinality :db.cardinality/many :db/isComponent true}
   :session/host      {:db/valueType :db.type/ref}
   :token/image       {:db/valueType :db.type/ref}
   :user/camera       {:db/valueType :db.type/ref}
   :user/cameras      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :user/dragging     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :user/image        {:db/valueType :db.type/ref}
   :user/uuid         {:db/unique :db.unique/identity}

   ;; ---- Player Sheet Schema Additions ---- 
   :player-sheet/id {:db/valueType   :db.type/ref
                     :db/unique      :db.unique/identity
                     :db/cardinality :db.cardinality/one
                     :db/doc         "Unique identifier for the player sheet"}
   
   :root/player-sheets {:db/valueType   :db.type/ref
                        :db/cardinality :db.cardinality/many
                        :db/isComponent true ; Optional, but common if sheets are "owned"
                        :db/doc         "References to all player sheet entities"}})

#_(def schema
  {;; Original Attributes (made more explicit)
   :camera/scene      {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :camera/selected   {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many}
   ;; :db/ident is a built-in attribute, no need to redefine its schema here.
   ;; DataScript knows :db/ident {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}
   :db/txInstant      {:db/valueType :db.type/instant, :db/cardinality :db.cardinality/one} ; Standard DataScript attribute, good to have if you query it

   :image/hash        {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity, :db/doc "Unique hash for an image"}
   :image/name        {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one} ; Added from scene-images/create-many
   :image/size        {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}   ; Added
   :image/width       {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}   ; Added
   :image/height      {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}  ; Added
   :image/scope       {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one} ; Added from token-images/create-many
   :image/thumbnail   {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/isComponent true}
   :image/thumbnail-rect {:db/valueType :db.type/any, :db/cardinality :db.cardinality/one} ; Assuming rect is a vector or map

   :initiative/played {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many}
   :initiative/turn   {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :initiative/roll   {:db/valueType :db.type/double, :db/cardinality :db.cardinality/one} ; Assuming float for roll
   :initiative/health {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :initiative/suffix {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :initiative/rounds {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}

   :root/scene-images {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :root/session      {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/isComponent true}
   :root/token-images {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :root/user         {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/isComponent true}
   :root/release      {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one} ; From initial-data

   :scene/image       {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :scene/initiative  {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many}
   :scene/masks       {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :scene/shapes      {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :scene/tokens      {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :scene/notes       {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :scene/grid-size   {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :scene/grid-origin {:db/valueType :db.type/any, :db/cardinality :db.cardinality/one} ; Assuming vec [x y]
   :scene/show-grid   {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :scene/dark-mode   {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :scene/grid-align  {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :scene/show-object-outlines {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :scene/lighting    {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
   :scene/masked      {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :db/empty          {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one} ; From initial-data for scenes

   :session/conns     {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :session/host      {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :session/room      {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one} ; From status-query
   :session/status    {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
   :session/share-cursors {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}

   :token/image       {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :token/label       {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :token/flags       {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/many} ; Set of keywords
   :token/size        {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :token/light       {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :token/aura-radius {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}

   :user/camera       {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :user/cameras      {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/isComponent true}
   :user/dragging     {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/many}
   :user/image        {:db/valueType :db.type/ref, :db/cardinality :db.cardinality/one}
   :user/uuid         {:db/valueType :db.type/uuid, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}
   :user/ready        {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one} ; From initial-data
   :user/color        {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}  ; From initial-data
   :user/type         {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one} ; From initial-data
   :user/label        {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :user/description  {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :user/bounds       {:db/valueType :db.type/any, :db/cardinality :db.cardinality/one} ; Assuming vec
   :user/clipboard    {:db/valueType :db.type/any, :db/cardinality :db.cardinality/one} ; Assuming collection
   :user/share-cursor {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}

   :panel/selected    {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one} ; From initial-data
   :panel/expanded    {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one} ; From initial-data

   ;; Object attributes (generic for tokens, shapes, notes)
   :object/type       {:db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
   :object/point      {:db/valueType :db.type/any, :db/cardinality :db.cardinality/one} ; Assuming vec [x y]
   :object/hidden     {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :object/locked     {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}

   ;; Note specific attributes
   :note/icon         {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :note/label        {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :note/description  {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}

   ;; Shape specific attributes
   :shape/points      {:db/valueType :db.type/any, :db/cardinality :db.cardinality/one} ; Assuming vector of points
   :shape/color       {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :shape/pattern     {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}

   ;; Mask specific attributes
   :mask/vecs         {:db/valueType :db.type/any, :db/cardinality :db.cardinality/one} ; From mask/create
   :mask/enabled?     {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}

   ;; ---- Player Sheet Schema Additions ---- 
   :player-sheet/id {:db/valueType   :db.type/uuid
                     :db/unique      :db.unique/identity
                     :db/cardinality :db.cardinality/one
                     :db/doc         "Unique identifier for the player sheet"}
   
   :root/player-sheets {:db/valueType   :db.type/ref
                        :db/cardinality :db.cardinality/many
                        :db/isComponent true ; Optional, but common if sheets are "owned"
                        :db/doc         "References to all player sheet entities"}

   :player-sheet/character-name {:db/valueType   :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Character's full name"}
   :player-sheet/class-level {:db/valueType   :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Character's class(es) and level(s)"}
   :player-sheet/background {:db/valueType   :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Character's background"}
   :player-sheet/player-name {:db/valueType   :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Name of the player"}
   :player-sheet/race {:db/valueType   :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Character's race"}
   :player-sheet/alignment {:db/valueType   :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Character's alignment"}
   :player-sheet/experience-points {:db/valueType   :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Character's current experience points"}

   :player-sheet/strength {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Strength score"}
   :player-sheet/dexterity {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Dexterity score"}
   :player-sheet/constitution {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Constitution score"}
   :player-sheet/intelligence {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Intelligence score"}
   :player-sheet/wisdom {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Wisdom score"}
   :player-sheet/charisma {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Charisma score"}

   :player-sheet/inspiration {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one, :db/doc "Whether the character has inspiration"}
   :player-sheet/proficiency-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one, :db/doc "Character's proficiency bonus"}

   :player-sheet/st-strength-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/st-strength-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/st-dexterity-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/st-dexterity-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/st-constitution-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/st-constitution-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/st-intelligence-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/st-intelligence-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/st-wisdom-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/st-wisdom-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/st-charisma-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/st-charisma-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}

   :player-sheet/skill-acrobatics-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-acrobatics-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-animal-handling-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-animal-handling-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-arcana-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-arcana-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-athletics-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-athletics-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-deception-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-deception-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-history-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-history-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-insight-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-insight-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-intimidation-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-intimidation-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-investigation-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-investigation-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-medicine-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-medicine-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-nature-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-nature-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-perception-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-perception-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-performance-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-performance-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-persuasion-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-persuasion-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-religion-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-religion-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-sleight-of-hand-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-sleight-of-hand-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-stealth-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-stealth-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-survival-prof {:db/valueType :db.type/boolean, :db/cardinality :db.cardinality/one}
   :player-sheet/skill-survival-bonus {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}

   :player-sheet/passive-perception {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/armor-class {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/initiative {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/speed {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/hp-max {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/hp-current {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/hp-temp {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/hit-dice-total {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/hit-dice-current {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/death-saves-successes {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}
   :player-sheet/death-saves-failures {:db/valueType :db.type/long, :db/cardinality :db.cardinality/one}

   :player-sheet/attacks-spellcasting {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Text block for attacks and spellcasting details"}
   :player-sheet/personality-traits {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/ideals {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/bonds {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/flaws {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/features-traits {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Text block for class/race features and traits"}
   :player-sheet/proficiencies-languages {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Text block for other proficiencies and languages"}
   :player-sheet/equipment {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "Text block for equipment"}
   :player-sheet/appearance {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/backstory {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/allies-organizations {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
   :player-sheet/treasure {:db/valueType :db.type/string, :db/cardinality :db.cardinality/one}

   :player-sheet/created-at {:db/valueType :db.type/instant, :db/cardinality :db.cardinality/one, :db/doc "Timestamp when the sheet was created"}
   :player-sheet/updated-at {:db/valueType :db.type/instant, :db/cardinality :db.cardinality/one, :db/doc "Timestamp when the sheet was last updated"}})

;; Rest of the state.cljs file (initial-data, context, listeners, persistence, provider, use-query)
;; ... remains the same as provided by the user ...

(defn initial-data [type]
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :root/release VERSION]
    [:db/add -1 :root/scenes -2]
    [:db/add -1 :root/user -3]
    [:db/add -1 :root/session -5]
    [:db/add -2 :db/empty true]
    [:db/add -3 :db/ident :user]
    [:db/add -3 :user/ready false]
    [:db/add -3 :user/color "red"]
    [:db/add -3 :user/camera -4]
    [:db/add -3 :user/cameras -4]
    [:db/add -3 :user/type type]
    [:db/add -3 :panel/selected :tokens]
    [:db/add -4 :camera/scene -2]
    [:db/add -4 :camera/point vec/zero]
    [:db/add -5 :db/ident :session]]))

(def context (uix/create-context))

(defui ^:private listeners []
  (let [write (idb/use-writer "images")]
    ;; Removes the given scene image and its thumbnail from the
    ;; IndexedDB images object store.
    (events/use-subscribe :scene-images/remove
      (uix/use-callback
       (fn [& hashes] (write :delete hashes)) [write]))

    ;; Removes the given token image and its thumbnail from the
    ;; IndexedDB images object store.
    (events/use-subscribe :token-images/remove
      (uix/use-callback
       (fn [& hashes] (write :delete hashes)) [write]))

    ;; Removes the given token images from the IndexedDB images
    ;; object store.
    (events/use-subscribe :token-images/remove-all
      (uix/use-callback
       (fn [hashes] (write :delete hashes)) [write]))))

(def ^:private ignored-attrs
  #{:user/type :user/ready :session/status})

(defui ^:private persistence [{:keys [type]}]
  (let [conn  (uix/use-context context)
        read  (idb/use-reader "app")
        write (idb/use-writer "app")]
    ;; Persists the DataScript state to IndexedDB whenever changes
    ;; are made to it.
    (uix/use-effect
     (fn []
       (ds/listen! conn :marshaller
         (throttle
          (fn [{:keys [db-after]}]
            (if (:user/ready (ds/entity db-after [:db/ident :user]))
              (-> db-after
                  (ds/db-with [[:db/retract [:db/ident :session] :session/host]
                               [:db/retract [:db/ident :session] :session/conns]])
                  (ds/filter (fn [_ [_ attr _ _]] (not (contains? ignored-attrs attr))))
                  (ds/datoms :eavt)
                  (as-> datoms (t/write writer datoms))
                  (as-> marshalled #js {:release VERSION :updated (* -1 (.now js/Date)) :data marshalled})
                  (as-> record (write :put [record])))))
          600))
       (fn [] (ds/unlisten! conn :marshaller))) [conn write])

    ;; Reads existing state from IndexedDB, if it exists, and replaces
    ;; the DataScript state with it.
    (uix/use-effect
     (fn []
       (let [tx-data
             [[:db/add [:db/ident :user] :user/ready true]
              [:db/add [:db/ident :user] :user/type type]]]
         (.then (read VERSION)
                (fn [record]
                  (if (nil? record)
                    (ds/transact! conn tx-data)
                    (-> (t/read reader (.-data record))
                        (ds/conn-from-datoms schema)
                        (ds/db)
                        (ds/db-with tx-data)
                        (as-> data (ds/reset-conn! conn data)))))))) ^:lint/disable [])))

(defui provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [{:keys [children type] :or {type :host}}]
  (let [[conn] (uix/use-state (ds/conn-from-db (initial-data type)))]
    ($ context {:value conn}
      (if (= type :host)
        ($ persistence {:type type}))
      ($ listeners)
      children)))

(defn use-query
  ([pattern]
   (use-query pattern [:db/ident :user]))
  ([pattern entity-id]
   (let [conn                   (uix/use-context context)
         get-result             (uix/use-callback #(ds/pull @conn pattern entity-id) ^:lint/disable [])
         [listen-key]           (uix/use-state random-uuid)
         [prev-state set-state] (uix/use-state get-result)]
     (uix/use-effect
      (fn []
        (ds/listen! conn listen-key
          (fn []
            (let [next-state (get-result)]
              (if (not= prev-state next-state)
                (set-state next-state)))))
        (fn []
          (ds/unlisten! conn listen-key))) ^:lint/disable [prev-state])
     prev-state)))
