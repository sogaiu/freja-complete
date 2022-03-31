(import freja/new_gap_buffer :as gb)
(import freja/event/subscribe :as s)
(import freja/hiccup :as h)
(import freja/state)
(import freja/theme)
(import freja/textarea :as ta)

(import ./utils :as u)

(defn replace-with-choice
  [gb [start end] choice]
  # minimal edit - only insert what's necessary so undoing looks ok
  (gb/insert-string-at-pos! gb end (string/slice choice (- end start)))
  # put cursor at end of newly inserted content
  (gb/put-caret gb (+ start (length choice))))

(defn completion-candidates-component
  [props]
  (def {:bounds bounds
        :candidates candidates
        :cleanup cleanup
        :gb gb
        :input input
        :offset offset}
    props)
  #
  (default offset 0)
  #
  (defn confirm
    [choice]
    (replace-with-choice gb bounds choice)
    (cleanup))
  #
  (def peg
    (if (or (nil? input)
            (empty? input))
      ~(capture (any 1))
      (u/search-peg input)))
  #
  (def filtered-candidates
    (filter |(not (empty? (peg/match peg $)))
            candidates))
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
           @{:extra-binds
             @{:escape (fn [_] (cleanup))
               :down (fn [_]
                       (let [new (inc offset)
                             new (if (>= new (length filtered-candidates))
                                   0
                                   new)]
                         (s/put! props :offset new)))
               :up (fn [_]
                     (let [new (dec offset)
                           new (if (< new 0)
                                 (dec (length filtered-candidates))
                                 new)]
                       (s/put! props :offset new)))
               :enter (fn [_]
                        (confirm selected-candidate))}
             :height 22
             :init (fn [self _]
                     (s/put! state/focus :focus (self :state)))
             :on-change |(s/put! props :input $)
             :text/color :white
             :text/size 20}]]
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

(defn make-cleanup-fn
  [layer-name]
  (def editor-state
    (state/focus :focus))
  #
  (fn []
    (h/remove-layer layer-name nil)
    # restore focus
    (s/put! state/focus :focus editor-state)))

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
  #
  (def src
    (gb/content gb))
  #
  (def layer-name :candidates)
  #
  (def candidates
    (u/enumerate-candidates src input))
  #
  (cond
    (and (= 1 (length candidates))
         (= (string input)
            (first candidates)))
    (eprintf "No other candidates found")
    #
    (< 1 (length candidates))
    (h/new-layer layer-name
                 completion-candidates-component
                 @{:bounds [start caret]
                   :candidates candidates
                   # XXX: if remove-layer starts using 2nd arg, put in component?
                   :cleanup (make-cleanup-fn layer-name)
                   :gb gb
                   :input input})
    # hard for this to happen
    (eprintf "No candidates found"))
  #
  gb)
