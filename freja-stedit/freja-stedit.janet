(import freja/new_gap_buffer :as gb)
(import freja/state)

(import ./stedit/stedit :as se)

# XXX: for investigation
(defn current-gb
  []
  (get-in state/editor-state [:stack 0 1 :editor :gb]))

(varfn point
  [gb]
  (gb :caret))

(varfn char-after
  [gb i]
  (gb/gb-nth gb i))

(varfn goto-char
  [gb i]
  (gb/put-caret gb i))

(varfn beginning-of-buffer?
  [gb]
  (= (gb :caret) 0))

(varfn end-of-buffer?
  [gb]
  (= (gb :caret) (gb/gb-length gb)))

(varfn backward-line
  [gb]
  (let [curr-line (gb/index-start-of-line gb (gb :caret))
        # checking not at beginning of buffer not needed
        prev-line (gb/index-start-of-line gb (dec curr-line))]
    (put gb :caret prev-line)))

(varfn forward-line
  [gb]
  (let [curr-line (gb/index-end-of-line gb (gb :caret))
        # XXX: checking not at end of buffer not needed?
        next-line (inc curr-line)]
    (put gb :caret next-line)))

# XXX: review
(varfn skip-whitespace-forward
  ``
  Skips forward while there is whitespace.
  ``
  [gb]
  (def {:caret caret} gb)

  (var target-i caret)
  (def start-i caret)

  (def f
    (fiber/new
      (fn []
        (gb/index-char-start gb start-i))))

  (loop [[i c] :iterate (resume f)]
    (when (and (not= (chr " ") c)
               (not= (chr "\t") c)
               (not= (chr "\n") c))
          (set target-i i)
          (break)))

  (if (> target-i (gb/gb-length gb))
    nil
    (gb/move-n gb (- target-i start-i))))

(varfn skip-whitespace-backward
  ``
  Skips backward while there is whitespace.
  ``
  [gb]
  (def {:caret caret} gb)

  (when (zero? caret)
    (break))

  (var target-i (dec caret))
  (def start-i (dec caret))

  (def f
    (fiber/new
      (fn []
        (gb/index-char-backward-start gb start-i))))

  (loop [[i c] :iterate (resume f)]
    (when (and (not= (chr " ") c)
               (not= (chr "\t") c)
               (not= (chr "\n") c))
      (set target-i i)
      (break)))

  (def diff
    (- target-i start-i))

  # XXX: does this cover all cases?
  (unless (= start-i target-i)
    (gb/move-n gb diff)))

(defn begin-of-top-level-char?
  [char]
  (def botl-chars
    {(chr "(") true
     (chr "~") true
     (chr "'") true})
  (get botl-chars char))

(varfn begin-of-top-level
  [gb]
  # XXX: necessary?
  (-> gb gb/commit!)
  #
  (when (not (beginning-of-buffer? gb))
    (var pos (point gb))
    (gb/beginning-of-line gb)
    (if (begin-of-top-level-char? (char-after gb (point gb)))
      (set pos (point gb))
      (while true
        (backward-line gb)
        (cond
          (begin-of-top-level-char? (char-after gb (point gb)))
          (do
            (set pos (point gb))
            (break))
          #
          (beginning-of-buffer? gb)
          (break))))
    (goto-char gb pos)))

(varfn before-next-top-level
  [gb]
  # XXX: necessary?
  (-> gb gb/commit!)
  #
  (when (not (end-of-buffer? gb))
    (var pos (point gb))
    (def last-line-number
      (gb/line-number gb (gb/gb-length gb)))
    (forward-line gb)
    (if (begin-of-top-level-char? (char-after gb (point gb)))
      (do
        (gb/backward-char gb)
        (set pos (point gb)))
      (while (< (gb/line-number gb (point gb))
                last-line-number)
        (forward-line gb)
        (cond
          (begin-of-top-level-char? (char-after gb (point gb)))
          (do
            (gb/backward-char gb)
            (set pos (point gb))
            (break))
          #
          (end-of-buffer? gb)
          (do
            (set pos (point gb))
            (break)))))
    (if (= (gb/line-number gb (point gb))
           last-line-number)
      (gb/end-of-buffer gb)
      (goto-char gb pos))))

(varfn absorb-forward
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to operate on
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to operate on
      (goto-char gb current)
      (before-next-top-level gb)
      # XXX: hoping this is enough...hmm, what about comments...
      (skip-whitespace-forward gb)
      (gb/forward-char gb)
      # going further to handle case of top-level absorb
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # replace if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             new-text (se/absorb-forward cursor-lc region)]
    # move out of the way of upcoming region deletion
    (goto-char gb start)
    (gb/delete-region! gb start end)
    (gb/insert-string-at-pos! gb start new-text)
    # restore cursor position -- XXX: hopefully this works?
    (goto-char gb current))
  gb)

(varfn find-pos-for-line-and-column
  [gb line column]
  # goto line
  # * determine current line
  (def current-line-number
    (gb/current-line-number gb))
  # * determine whether destination line is above, below, or some
  #  and adjust line if necessary
  (cond
    (< line current-line-number)
    (for i 0 (- current-line-number line)
      (backward-line gb))
    #
    (> line current-line-number)
    (for i 0 (- line current-line-number)
      (forward-line gb))
    #
    nil)
  (var pos (point gb))
  # goto column
  # * determine current column
  (def current-column (gb/column! gb pos))
  # * adjust value using gb/move-n
  (gb/move-n gb (- column current-column))
  # get position
  (point gb))

(varfn eject-forward
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to operate on
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to operate on
      (goto-char gb current)
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # replace if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             [new-text [new-l-1 new-c-1]]
             (se/eject-forward cursor-lc region)
             # 0-based
             [new-l-o new-c-o] [(dec new-l-1) (dec new-c-1)]
             # offset
             [new-l new-c] [(+ new-l-o start-l) new-c-o]]
    # move out of the way of upcoming region deletion
    (goto-char gb start)
    (gb/delete-region! gb start end)
    (gb/insert-string-at-pos! gb start new-text)
    # possibly adjust cursor position -- XXX: hopefully this works?
    (goto-char gb
               (find-pos-for-line-and-column gb new-l new-c)))
  gb)

(varfn forward-expr
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      # XXX: hoping this is enough...hmm, what about comments...
      (skip-whitespace-forward gb)
      (gb/forward-char gb)
      # going further to handle case of top-level absorb
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # move forward if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             [new-l-1 new-c-1] (se/forward-expr cursor-lc region)
             # 0-based
             [new-l-o new-c-o] [(dec new-l-1) (dec new-c-1)]
             # offset
             [new-l new-c] [(+ new-l-o start-l) new-c-o]]
    (goto-char gb
               (find-pos-for-line-and-column gb new-l new-c)))
  gb)

(varfn backward-expr
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      # XXX: hoping this is enough...hmm, what about comments...
      (skip-whitespace-backward gb)
      (gb/backward-char gb)
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # move backward if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             [new-l-1 new-c-1] (se/backward-expr cursor-lc region)
             # 0-based
             [new-l-o new-c-o] [(dec new-l-1) (dec new-c-1)]
             # offset
             [new-l new-c] [(+ new-l-o start-l) new-c-o]]
    (goto-char gb
               (find-pos-for-line-and-column gb new-l new-c)))
  gb)

(varfn forward-atom
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      # XXX: hoping this is enough...hmm, what about comments...
      (skip-whitespace-forward gb)
      (gb/forward-char gb)
      # going further to handle case of top-level absorb
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # move forward if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             [new-l-1 new-c-1] (se/forward-atom cursor-lc region)
             # 0-based
             [new-l-o new-c-o] [(dec new-l-1) (dec new-c-1)]
             # offset
             [new-l new-c] [(+ new-l-o start-l) new-c-o]]
    (goto-char gb
               (find-pos-for-line-and-column gb new-l new-c)))
  gb)

# XXX: can have issues within a form on the first line of the file
(varfn backward-atom
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      # XXX: hoping this is enough...hmm, what about comments...
      (skip-whitespace-backward gb)
      (gb/backward-char gb)
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # move forward if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             [new-l-1 new-c-1] (se/backward-atom cursor-lc region)
             # 0-based
             [new-l-o new-c-o] [(dec new-l-1) (dec new-c-1)]
             # offset
             [new-l new-c] [(+ new-l-o start-l) new-c-o]]
    (goto-char gb
               (find-pos-for-line-and-column gb new-l new-c)))
  gb)

(varfn forward-down-expr
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      (skip-whitespace-forward gb)
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # move forward and down if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             new-lc-maybe (se/forward-down-expr cursor-lc (tracev region))
             [new-l-1 new-c-1] new-lc-maybe
             # 0-based
             [new-l-o new-c-o] [(dec new-l-1) (dec new-c-1)]
             # offset
             [new-l new-c] [(+ new-l-o start-l) new-c-o]]
    (goto-char gb
               (find-pos-for-line-and-column gb new-l new-c)))
  gb)

(varfn backward-up-expr
  [gb]
  (def current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # move backward and up if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             new-lc-maybe (se/backward-up-expr cursor-lc region)
             [new-l-1 new-c-1] new-lc-maybe
             # 0-based
             [new-l-o new-c-o] [(dec new-l-1) (dec new-c-1)]
             # offset
             [new-l new-c] [(+ new-l-o start-l) new-c-o]]
    (goto-char gb
               (find-pos-for-line-and-column gb new-l new-c)))
  gb)

(varfn backward-start-of-top-level
  [gb]
  (gb/backward-char gb)
  (begin-of-top-level gb)
  gb)

(varfn forward-end-of-top-level
  [gb]
  (gb/forward-char gb)
  (before-next-top-level gb)
  gb)

(varfn delete-forward-expr
  [gb]
  (def original (point gb))
  # the following skipping is for coping with top-level situations.
  # it will be accounted for later if it turns out the starting point was
  # not at the top-level
  (skip-whitespace-forward gb)
  (var current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # not at top-level, so adjust current to match original
  (when (not (< original start))
    (set current original))
  # delete region if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             [end-l-1 end-c-1] (se/delete-forward-expr cursor-lc region)
             # 0-based
             [end-l-o end-c-o] [(dec end-l-1) (dec end-c-1)]
             # offset
             [end-l end-c] [(+ end-l-o start-l) end-c-o]
             new-end (find-pos-for-line-and-column gb end-l end-c)]
    # move out of the way of upcoming region deletion
    (goto-char gb current)
    (gb/delete-region! gb current new-end))
  # restore cursor position
  (goto-char gb original)
  gb)

(varfn select-forward-expr
  [gb]
  (def original (point gb))
  # the following skipping is for coping with top-level situations.
  # it will be accounted for later if it turns out the starting point was
  # not at the top-level
  (skip-whitespace-forward gb)
  (var current (point gb))
  # find bounds of enough text
  (def [start end]
    (defer (goto-char gb current)
      # find and remember beginning of region to examine
      (begin-of-top-level gb)
      (def start (point gb))
      # find and remember end of region to examine
      (goto-char gb current)
      (before-next-top-level gb)
      (def end (point gb))
      [start end]))
  # not at top-level, so adjust current to match original
  (when (not (< original start))
    (set current original))
  # doing this here in case the upcoming selection doesn't happen
  (goto-char gb original)
  # select region if appropriate
  (when-let [curr-l (gb/line-number gb current)
             curr-c (gb/column! gb current)
             start-l (gb/line-number gb start)
             # 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             region (string/slice (gb/content gb) start end)
             # 1-based
             [end-l-1 end-c-1] (se/delete-forward-expr cursor-lc region)
             # 0-based
             [end-l-o end-c-o] [(dec end-l-1) (dec end-c-1)]
             # offset
             [end-l end-c] [(+ end-l-o start-l) end-c-o]
             new-end (find-pos-for-line-and-column gb end-l end-c)]
    # restore cursor position before selecting
    (goto-char gb original)
    (gb/select-region gb current new-end))
  gb)



