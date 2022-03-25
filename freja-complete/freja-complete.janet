(import freja/new_gap_buffer :as gb)
(import freja/events :as e)
(import freja/hiccup :as h)
(import freja/state)
(import freja/theme)
(import freja/textarea :as ta)

(import ./utils :as u)

(defn insert-completion
  [gb start end choice]
  (gb/delete-region! gb start end)
  (gb/insert-string-at-pos! gb start choice)
  (gb/put-caret gb (+ start (length choice)))
  gb)

(defn completion-candidates-component
  [props]
  (def {:gb gb
        :input input
        :candidates candidates
        :offset offset
        :start start}
    props)
  #
  (default offset 0)
  #
  (defn confirm
    [choice]
    (insert-completion gb start (+ start (length input)) choice)
    (h/remove-layer :candidates props))
  #
  (def filtered-candidates
    (->> candidates
         # XXX
         ))
  #
  (def offset
    (-> offset
        (max 0)
        (min (dec (length filtered-candidates)))))
  #
  (def selected-candidate
    (get filtered-candidates offset))
  #
  [:padding {:all 0}
   [:column {}
    [:block {:weight 0.05}]
    [:row {}
     [:block {:weight 0.5}]
     [:block {:weight 1}
      [:clickable {:on-click
                   # only done to stop clicks from passing through
                   (fn [_])}
       [:background {:color (theme/comp-cols :background)}
        [:padding {:all 4}
         [:block {} [:text {:size 24
                            :color (theme/comp-cols :text/color)
                            :text "Select candidate"}]]
         [:padding {:top 6 :bottom 6}
          [ta/textarea
           @{:text/color :white
             :init (fn [self _]
                     (e/put! state/focus :focus (self :state)))
             :text/size 20
             :height 22
             :extra-binds
             @{:escape (fn [_]
                         (h/remove-layer :candidates props)
                         (:freja/focus
                           (in (last (state/editor-state :stack)) 1)))
               :down (fn [_]
                       (let [new (inc offset)
                             new (if (>= new (length filtered-candidates))
                                   0
                                   new)]
                         (e/put! props :offset new)))
               :up (fn [_]
                     (let [new (dec offset)
                           new (if (< new 0)
                                 (dec (length filtered-candidates))
                                 new)]
                             (e/put! props :offset new)))
               :enter (fn [_]
                        (confirm selected-candidate))}
             # XXX
             #:on-change |(e/put! props :input $)
             }]]
         [:background {:color (theme/comp-cols :bar-bg)}
          ;(seq [c :in filtered-candidates
                 :let [selected (= c selected-candidate)]]
             [:clickable {:on-click (fn [_] (confirm c))}
              (if selected
                [:background {:color 0xffffff99}
                 [:block {}
                  [:padding {:all 2}
                   [:text {:color 0x111111ff
                           :size 16
                           :text (or selected-candidate "")}]]]]
                [:block {}
                 [:padding {:all 2}
                  [:text {:text c
                          :size 16
                          :color :white}]]])])]]]]]
     [:block {:weight 0.5}]]
    [:block {:weight 1}]]])

(varfn complete
  [gb]
  (def caret (gb :caret))
  (def chars
    {(chr " ") 1
     (chr "\t") 1
     (chr "\n") 1
     (chr "(") 1
     (chr ")") 1
     (chr "{") 1
     (chr "}") 1
     (chr "[") 1
     (chr "]") 1
     (chr `"`) 1
     (chr "'") 1
     (chr "`") 1})
  (def start
    (gb/search-backward gb
                        (fn [c] (get chars c))
                        caret))
  (when (>= start caret)
    (eprintf "%p not less than %p" start caret)
    (break gb))
  #
  (def input
    (gb/gb-slice gb start caret))
  (def src
    (gb/content gb))
  #
  (def state
    @{:candidates (u/enumerate-candidates src input)
      :gb gb
      :input input
      :start start})
  #
  (h/new-layer :candidates
               completion-candidates-component
               state)
  #
  gb)
