(defn line-to-items
  [line]
  (->> line
       (peg/replace-all ~(choice "[" "]"
                                 "(" ")"
                                 "{" "}"
                                 `"` `'` "`")
                        " ")
       (string/split " ")
       (keep (fn [item]
               (when (not (empty? item))
                 item)))
       distinct))

(comment

  (line-to-items "(let [curr-line (gb/index-end-of-line gb (gb :caret))")
  # =>
  @["let"
    "curr-line"
    "gb/index-end-of-line"
    "gb"
    ":caret"]

  )

(defn enumerate-candidates
  [src input]
  (def lines
    (string/split "\n" src))
  (def cands @[])
  #
  (each l lines
    (each item (line-to-items l)
      (when (string/has-prefix? input item)
        (array/push cands item))))
  #
  (sort (distinct cands)))

(comment

  (def src
    ``
    (import freja/new_gap_buffer :as gb)
    (import freja/state)
    (import freja/theme)
    (import freja/textarea :as t)

    # XXX: for investigation
    (defn current-gb
      []
      (get-in state/editor-state [:stack 0 1 :editor :gb]))
    ``)

  (enumerate-candidates src ":")
  # =>
  @[":as"
    ":editor"
    ":gb"
    ":stack"]

  (enumerate-candidates src "f")
  # =>
  @["for"
    "freja/new_gap_buffer"
    "freja/state"
    "freja/textarea"
    "freja/theme"]

  (enumerate-candidates src "g")
  # =>
  @["gb" "get-in"]

  )
