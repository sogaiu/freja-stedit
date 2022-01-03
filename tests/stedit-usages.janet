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

# absorb-right
(comment

  (def src
    ``
    [] :a :b :x :y
    ``)

  (absorb-right [1 1] src)
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

  (absorb-right [3 4] src)
  # =>
  ``
  (comment

    [:a] :b :x :y

    )

  (comment

    )

  ``

  )
