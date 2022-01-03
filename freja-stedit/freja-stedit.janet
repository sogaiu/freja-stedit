(import freja/new_gap_buffer :as gb)
(import freja/state)
(import freja/default-hotkeys :as dh)

(import ../stedit/stedit :as se)

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
  "Skips forward while there is whitespace on current line."
  [gb]
  (def {:caret caret} gb)

  (var target-i caret)
  (def start-i caret)

  (def f
    (fiber/new
      (fn []
        (gb/index-char-start gb start-i))))

  (when (= (chr "\n") (char-after gb caret))
    (break nil))

  (loop [[i c] :iterate (resume f)]
    (when (and (not= (chr " ") c)
               (not= (chr "\t") c))
      (set target-i i)
      (break)))

  (if (> target-i (gb/gb-length gb))
    nil
    (gb/move-n gb (- target-i start-i))))

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

(varfn absorb-right
  [gb]
  (def current (point gb))
  (def curr-l
    (gb/line-number gb current))
  (def curr-c
    (gb/column! gb current))
  (var start nil)
  (var start-l nil)
  (var end nil)
  # find bounds of enough text
  (defer (goto-char gb current)
    # find and remember beginning of region to operate on
    (begin-of-top-level gb)
    (set start (point gb))
    (set start-l (gb/line-number gb start))
    # find and remember end of region to operate on
    (goto-char gb current)
    (before-next-top-level gb)
    # XXX: hoping this is enough...hmm, what about comments...
    (skip-whitespace-forward gb)
    (gb/forward-char gb)
    # going further to handle case of top-level absorb
    (before-next-top-level gb)
    (set end (point gb)))
  (def region
       (string/slice (gb/content gb) start end))
  # XXX
  (printf "start-l: %p" start-l)
  (printf "curr-l: %p" curr-l)
  (printf "curr-c: %p" curr-c)
  (printf "region: %p" region)
  # compute replacement text
  # only replace if successful
  (when-let [# 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             new-text (se/absorb-right cursor-lc region)]
    # XXX
    (printf "cursor-lc (1-based): %p" cursor-lc)
    (printf "new-text: %p" new-text)
    # move out of the way of upcoming region deletion
    (goto-char gb start)
    (gb/delete-region! gb start end)
    (gb/insert-string-at-pos! gb start new-text)
    # restore cursor position -- XXX: hopefully this works?
    (goto-char gb current))
  gb)

(put-in dh/gb-binds
        # XXX: this is control right paren
        [:control :shift :0]
        (comp dh/reset-blink absorb-right))

(varfn eject-right
  [gb]
  (def current (point gb))
  (def curr-l
    (gb/line-number gb current))
  (def curr-c
    (gb/column! gb current))
  (var start nil)
  (var start-l nil)
  (var end nil)
  # find bounds of enough text
  (defer (goto-char gb current)
    # find and remember beginning of region to operate on
    (begin-of-top-level gb)
    (set start (point gb))
    (set start-l (gb/line-number gb start))
    # find and remember end of region to operate on
    (goto-char gb current)
    (before-next-top-level gb)
    (set end (point gb)))
  (def region
       (string/slice (gb/content gb) start end))
  # XXX
  (printf "start-l: %p" start-l)
  (printf "curr-l: %p" curr-l)
  (printf "curr-c: %p" curr-c)
  (printf "region: %p" region)
  # compute replacement text
  # only replace if successful
  (when-let [# 1-based line and column for zipper
             cursor-lc [(inc (- curr-l start-l))
                        (inc curr-c)]
             new-text (se/eject-right cursor-lc region)]
    # XXX
    (printf "cursor-lc (1-based): %p" cursor-lc)
    (printf "new-text: %p" new-text)
    # move out of the way of upcoming region deletion
    (goto-char gb start)
    (gb/delete-region! gb start end)
    (gb/insert-string-at-pos! gb start new-text)
    # restore cursor position -- XXX: hopefully this works?
    (goto-char gb current))
  gb)

(put-in dh/gb-binds
        # XXX: this is control right bracket
        [:control :shift :right-bracket]
        (comp dh/reset-blink eject-right))

(comment

  [] :a :b :x :y

  )

(comment

  )
