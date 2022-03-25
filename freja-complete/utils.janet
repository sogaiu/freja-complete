(defn case-insensitive-peg
  [str]
  ~(sequence
     ,;(map (fn [c]
              ~(choice ,(string/ascii-upper (string/from-bytes c))
                       ,(string/ascii-lower (string/from-bytes c))))
            str)))

(comment

  (peg/match (case-insensitive-peg "cat") "CAT")
  #=>
  @[]

  (peg/match (case-insensitive-peg "cAT") "Cat")
  #=>
  @[]

  )

(defn search-peg
  ``
  Given a string `search`, returns a peg that finds start positions of
  that string.

  Matches by splitting `search` by spaces, and where each space was,
  anything matches.
  ``
  [search]
  (def parts (string/split " " search))
  (var parts-peg @[])
  #
  (loop [i :range [0 (length parts)]
         :let [p (in parts i)
               p-peg (case-insensitive-peg p)
               p2 (get parts (inc i))
               p2-peg (when p2
                        (case-insensitive-peg p2))]]
    (array/push parts-peg p-peg)
    (array/push parts-peg
                (if p2-peg
                  ~(any (if-not ,p2-peg 1))
                  ~(any 1))))
  #
  ~{:main (any (choice :parts
                       1))
    :parts (sequence (position)
                     ,;parts-peg)})

(comment

  (peg/match (search-peg "fi do") "fine dog")
  #=>
  @[0]

  (peg/match (search-peg "f f") "firefox")
  #=>
  @[0]

  )

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
