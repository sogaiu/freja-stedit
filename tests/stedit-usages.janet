(import ../freja-stedit/stedit/location :as l)
(import ../freja-stedit/stedit/zipper :as j)
(import ../freja-stedit/stedit/loc-jipper :as j)
(import ../freja-stedit/stedit/stedit :prefix "")

# container?
(comment

  (-> (l/ast "@[1 1 2]")
      j/zip-down
      container?)
  # =>
  true

  (-> (l/ast "@(:x :y :z)")
      j/zip-down
      container?)
  # =>
  true

  (-> (l/ast "{:a 1}")
      j/zip-down
      container?)
  # =>
  true

  (-> (l/ast "@{:a 1}")
      j/zip-down
      container?)
  # =>
  true

  )

# find-container-for-lc
(comment

  (def src
    ``
    (comment

      [] :a :b :x :y

      )

    (comment

      )

    ``)

  (-> (l/ast src)
      j/zip-down
      (find-container-for-lc 3 4)
      j/node)
  # =>
  '(:bracket-tuple @{:bc 3 :bl 3 :ec 5 :el 3})

  )

# absorb-forward
(comment

  (def src
    ``
    [] :a :b :x :y
    ``)

  (absorb-forward [1 1] src)
  # =>
  ``
  [:a] :b :x :y
  ``

  (def src
    ``
    (comment

      [] :a :b :x :y

      )

    (comment

      )

   ``)

  (absorb-forward [3 4] src)
  # =>
  ``
  (comment

    [:a] :b :x :y

    )

  (comment

    )

  ``

  )

# eject-forward
(comment

  (def src
    "[:a :b :x] :y")

  (eject-forward [1 8] src)
  # =>
  '("[:a :b] :x :y" [1 7])

  (def src
    "[:a :b :x :y]")

  (eject-forward [1 8] src)
  # =>
  '("[:a :b :x] :y" (1 8))

  )

# forward-expr
(comment

  (def src
    ":a :b")

  (forward-expr [1 1] src)
  # =>
  [1 3]

  (def src
    "[:a :b]")

  (forward-expr [1 1] src)
  # =>
  [1 8]

  (def src
    "[:a :b] :c")

  (forward-expr [1 8] src)
  # =>
  [1 11]

  (def src
    "[:a :b :c]")

  (forward-expr [1 7] src)
  # =>
  [1 10]

  (def src
    "{:x [:a :b :c]}")

  (forward-expr [1 11] src)
  # =>
  [1 14]

  (def src
    ``
    (import freja/new_gap_buffer :as gb)
    (import freja/state)
    (import freja/default-hotkeys :as dh)
    ``)

  (forward-expr [1 1] src)
  # =>
  [1 37]

  (forward-expr [1 37] src)
  # =>
  [2 21]

  )

# backward-expr
(comment

  (def src
    ":a :b")

  (backward-expr [1 4] src)
  # =>
  [1 1]

  (def src
    "[:a :b]")

  (backward-expr [1 5] src)
  # =>
  [1 2]

  (def src
    "[:a :b] :c")

  (backward-expr [1 9] src)
  # =>
  [1 1]

  (def src
    "[:a :b :c]")

  (backward-expr [1 6] src)
  # =>
  [1 5]

  (def src
    "{:x [:a :b :c]}")

  (backward-expr [1 14] src)
  # =>
  [1 12]

  )

# forward-atom
(comment

  (def src
    ``
    (import freja/new_gap_buffer :as gb)
    (import freja/state)
    (import freja/default-hotkeys :as dh)
    ``)

  (forward-atom [1 1] src)
  # =>
  [1 8]

  (forward-atom [1 8] src)
  # =>
  [1 29]

  (forward-atom [1 29] src)
  # =>
  [1 33]

  (forward-atom [1 33] src)
  # =>
  [1 36]

  (forward-atom [1 36] src)
  # =>
  [2 8]

  (forward-atom [2 8] src)
  # =>
  [2 20]

  (forward-atom [2 20] src)
  # =>
  [3 8]

  )
