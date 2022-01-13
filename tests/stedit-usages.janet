(import ../stedit/location :as l)
(import ../stedit/zipper :as j)
(import ../stedit/loc-jipper :as j)
(import ../stedit/stedit :prefix "")

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
  [1 6]

  (def src
    "[:a :b]")

  (forward-expr [1 1] src)
  # =>
  nil

  (def src
    "[:a :b] :c")

  (forward-expr [1 1] src)
  # =>
  [1 11]

  (def src
    "[:a :b :c]")

  (forward-expr [1 6] src)
  # =>
  [1 10]

  (def src
    "{:x [:a :b :c]}")

  (forward-expr [1 9] src)
  # =>
  [1 14]

  )
