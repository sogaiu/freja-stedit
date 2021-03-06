(import ./location :as l)
(import ./zipper :as j)
(import ./loc-jipper :as j)

(defn spans?
  [[s-line-a s-col-a e-line-a e-col-a]
   [s-line-b s-col-b e-line-b e-col-b]]
  #
  (defn before?
    [line-a col-a line-b col-b]
    (cond
      (< line-a line-b)
      true
      #
      (= line-a line-b)
      (<= col-a col-b)
      #
      false))
  #
  (and (before? s-line-a s-col-a
                s-line-b s-col-b)
       (before? e-line-b e-col-b
                e-line-a e-col-a)))

(defn lc-for-zloc
  [zloc]
  (def {:bl bl
        :bc bc
        :el el
        :ec ec}
    (j/attrs zloc))
  [bl bc el ec])

(defn find-zloc-for-lc
  [zloc [l c]]
  (def hits @[])
  (def target-lc [l c l c])
  (var curr-zloc zloc)
  (while (not (j/end? curr-zloc))
    (let [curr-lc (lc-for-zloc curr-zloc)]
      (when (spans? curr-lc target-lc)
        (array/push hits curr-zloc))
      (set curr-zloc (j/df-next curr-zloc))))
  # XXX: not sure if this is right
  (last hits))

(comment

  (def src
    ``
    (defn my-fn
      [x]
      (+ x 1) :a
      )

    (def a 2)
    ``)

  (-> (find-zloc-for-lc (-> (l/ast src)
                            j/zip-down)
                        [3 5])
      j/node)
  # =>
  '(:whitespace @{:bc 5 :bl 3 :ec 6 :el 3} " ")

  (-> (find-zloc-for-lc (-> (l/ast src)
                            j/zip-down)
                        [6 5])
      j/node)
  # =>
  '(:whitespace @{:bc 5 :bl 6 :ec 6 :el 6} " ")

  (-> (find-zloc-for-lc (-> (l/ast src)
                            j/zip-down)
                        [6 6])
      j/node)
  # =>
  '(:symbol @{:bc 6 :bl 6 :ec 7 :el 6} "a")

  (-> (find-zloc-for-lc (-> (l/ast src)
                            j/zip-down)
                        [6 7])
      j/node)
  # =>
  '(:whitespace @{:bc 7 :bl 6 :ec 8 :el 6} " ")

  (-> (find-zloc-for-lc (-> (l/ast "[]")
                            j/zip-down)
                        [1 1])
      j/node)
  # =>
  '(:bracket-tuple @{:bc 1 :bl 1 :ec 3 :el 1})

  )

(defn find-absorbee
  [container-zloc]
  # look to the right, skipping whitespace
  # if comment is found, abort (not skip)
  (var curr-zloc (j/right container-zloc))
  (var result nil)
  (var cnt 0)
  (while curr-zloc
    (match (j/node curr-zloc)
      # abort
      [:comment]
      (break)
      # skip
      [:whitespace]
      nil
      # absorbee
      (do
        (set result curr-zloc)
        (++ cnt)
        (break)))
    (++ cnt)
    (set curr-zloc
         (j/right curr-zloc)))
  (if result
    [result cnt]
    nil))

(comment

  (def src
    ``
    [:a :b] :c
    ``)

  (def [absorbee-zloc cnt]
    (-> (l/ast src)
        j/zip-down
        find-absorbee))

  [(j/node absorbee-zloc) cnt]
  # =>
  '((:keyword @{:bc 9 :bl 1 :ec 11 :el 1} ":c") 2)

  (def src
    ``
    [:a :b] # hi
    :c
    ``)

  (-> (l/ast src)
      j/zip-down
      find-absorbee)
  # =>
  nil

  (def src
    ``
    [:a :b]
    :c
    ``)

  (def [absorbee-zloc cnt]
    (-> (l/ast src)
        j/zip-down
        find-absorbee))

  [(j/node absorbee-zloc) cnt]
  # =>
  '((:keyword @{:bc 1 :bl 2 :ec 3 :el 2} ":c") 2)

  )

(defn container?
  ``
  Returns true if `zloc` is a container node, false otherwise.

  A container node is one of the following types:

  * array
  * bracket array
  * tuple
  * bracket tuple
  * table
  * struct
  ``
  [zloc]
  (->> (first (j/node zloc))
       (get {:array true
             :bracket-array true
             :tuple true
             :bracket-tuple true
             :table true
             :struct true})
       truthy?))

(comment

  (-> (l/ast "(a)")
      j/zip-down
      container?)
  # =>
  true

  (-> (l/ast "[:a :b]")
      j/zip-down
      container?)
  # =>
  true

  (-> (l/ast ":a")
      j/zip-down
      container?)
  # =>
  false

  )

(defn find-container-for-lc
  ``
  Within `zloc`, return the z-location with the smallest container node
  spanning the location given by line `l` and column `c`.
  ``
  [zloc l c]
  (def a-zloc
    (find-zloc-for-lc zloc [l c]))
  (unless a-zloc
    (eprintf "did not find zloc for: [%p %p]" l c)
    (break nil))
  #
  (if (container? a-zloc)
    a-zloc
    (j/up a-zloc)))

(comment

  (-> (l/ast "[]")
      j/zip-down
      (find-container-for-lc 1 1)
      j/node)
  # =>
  '(:bracket-tuple @{:bc 1 :bl 1 :ec 3 :el 1})

  (-> (l/ast "[]")
      j/zip-down
      (find-container-for-lc 1 2)
      j/node)
  # =>
  '(:bracket-tuple @{:bc 1 :bl 1 :ec 3 :el 1})

  )

# XXX: could make a more general thing that doesn't depend on details
#      of janet-zipper, but not doing so for the moment
(defn remove-left-inclusive-n
  ``
  Remove `n` nodes ranging from `zloc` (inclusive) through n-1 left
  nodes.

  It's the caller's responsibility to ensure that `n` is a
  sensible value.
  ``
  [zloc n]
  (var curr-zloc zloc)
  # arrange for the left n-1 nodes to be non-container nodes
  (for i 0 (dec n)
    (set curr-zloc
         (j/left curr-zloc)))
  (for i 0 (dec n)
    (set curr-zloc
         (-> curr-zloc
             (j/replace [:whitespace @{} " "])
             j/right)))
  # now remove n nodes
  (for i 0 n
    (set curr-zloc
         (j/remove curr-zloc)))
  # XXX: it's kind of unclear where the current location is...
  curr-zloc)

(comment

  (-> (l/ast "[:a :b] :x :y :z")
      j/zip-down
      j/right
      j/right
      j/right
      j/right
      # at :y
      (remove-left-inclusive-n (+ 2 2))
      j/root
      l/code)
  # =>
  "[:a :b] :z"

  (-> (l/ast "[:a :b] :x :y :z")
      j/zip-down
      j/down
      j/right
      j/right
      # at :b
      (remove-left-inclusive-n (+ 2 1))
      j/root
      l/code)
  # =>
  "[] :x :y :z"

  )

(defn absorb-forward
  [[cursor-l cursor-c] src]
  (eprintf "src: %p" src)
  (var curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node for src: %p" (j/node curr-zloc))
  # find container
  (def container-zloc
    (find-container-for-lc curr-zloc cursor-l cursor-c))
  (unless (container? container-zloc)
    (eprintf "did not find container")
    (break nil))
  (eprintf "container node: %p" (j/node container-zloc))
  # find what to absorb
  (def absorbee-pair
    (find-absorbee container-zloc))
  (unless absorbee-pair
    (eprintf "did not find absorbee (1)")
    (break nil))
  (def absorbee-node
    (j/node (get absorbee-pair 0)))
  # absorb absorbee
  (set curr-zloc container-zloc)
  (when (pos? (length (j/children curr-zloc)))
    (eprintf "container node: %p" (j/node curr-zloc))
    (set curr-zloc
         (j/append-child curr-zloc [:whitespace @{} " "])))
  (set curr-zloc
       (j/append-child curr-zloc absorbee-node))
  (unless curr-zloc
    (eprintf "did not absorb absorbee")
    (break nil))
  # remove original absorbee
  (def absorbee-pair
    (find-absorbee curr-zloc))
  (unless absorbee-pair
    (eprintf "did not find absorbee (2)")
    (break nil))
  # remove nodes starting at the original absorbee and back to one
  # after the container
  (def [absorbee-zloc cnt] absorbee-pair)
  (eprintf "node for absorbee: %p" (j/node absorbee-zloc))
  (eprintf "cnt: %p" cnt)
  (set curr-zloc
       (remove-left-inclusive-n absorbee-zloc cnt))
  # replacement text
  (def new-text
    (-> curr-zloc
        j/root
        l/code))
  (eprintf "new-text: %p" new-text)
  new-text)

(comment

  (def src
    ``
    (defn my-fn
      [x]
      (+ x 1) :a)

    (def a 2)
    ``)

  (absorb-forward [3 6] src)
  # =>
  ``
  (defn my-fn
    [x]
    (+ x 1 :a))

  (def a 2)
  ``

  (def src
    ``
    (defn my-fn
      [x]
      (+ x 1) :a :b)

    (def a 2)
    ``)

  (->> src
       (absorb-forward [3 6])
       (absorb-forward [3 6]))
  # =>
  ``
  (defn my-fn
    [x]
    (+ x 1 :a :b))

  (def a 2)
  ``

  )

(defn find-ejectee
  [container-zloc]
  # start at the rightmost child of the container
  (var curr-zloc
    (-> (j/down container-zloc)
        j/rightmost))
  (unless curr-zloc
    (break nil))
  # look to the left, skipping whitespace
  # if comment is found, abort (not skip)
  (var result nil)
  (while curr-zloc
    (match (j/node curr-zloc)
      # abort
      [:comment]
      (break)
      # skip
      [:whitespace]
      nil
      # absorbee
      (do
        (set result curr-zloc)
        (break)))
    (set curr-zloc
         (j/left curr-zloc)))
  result)

(comment

  (def src
    ``
    [:a :b] :c
    ``)

  (def ejectee-zloc
    (-> (l/ast src)
        j/zip-down
        find-ejectee))

  (j/node ejectee-zloc)
  # =>
  '(:keyword @{:bc 5 :bl 1 :ec 7 :el 1} ":b")

  )

(defn eject-forward
  [[cursor-l cursor-c] src]
  (var curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  # find container
  (def container-zloc
    (find-container-for-lc curr-zloc cursor-l cursor-c))
  (unless (container? container-zloc)
    (eprintf "did not find container")
    (break nil))
  (eprintf "container node: %p" (j/node container-zloc))
  # find what to eject
  (def ejectee-zloc
    (find-ejectee container-zloc))
  (unless ejectee-zloc
    (eprintf "did not find ejectee (1)")
    (break nil))
  (def ejectee-node
    (j/node ejectee-zloc))
  # eject ejectee
  (set curr-zloc
       (-> container-zloc
           (j/insert-right ejectee-node)
           (j/insert-right [:whitespace @{} " "])))
  (unless curr-zloc
    (eprintf "did not eject ejectee")
    (break nil))
  # remove original ejectee
  (def ejectee-zloc
    (find-ejectee curr-zloc))
  (unless ejectee-zloc
    (eprintf "did not find ejectee (2)")
    (break nil))
  # remove nodes starting at the original ejectee and up through
  # but not including the previous / left non-whitespace / non-comment
  # node
  (eprintf "node: %p" (j/node ejectee-zloc))
  # XXX: this should work because if there's something to the left
  #      it should be whitespace, and if there's nothing to the
  #      left, there's nothing else to remove anyway
  (set curr-zloc
       (-> ejectee-zloc
           j/remove))
  (when (j/left ejectee-zloc)
    (def temp-zloc curr-zloc)
    (var marked 0)
    (while curr-zloc
      (match (j/node curr-zloc)
        [:whitespace]
        (++ marked)
        #
        [:comment]
        (errorf "unexpected :comment: %p" (j/node curr-zloc))
        #
        (break))
      (if-let [left-zloc (j/left curr-zloc)]
        (set curr-zloc left-zloc)
        (break)))
    (set curr-zloc temp-zloc)
    (for i 0 marked
      (set curr-zloc
           (j/remove curr-zloc))))
  # replacement text
  (def new-text
    (-> curr-zloc
        j/root
        l/code))
  (eprintf "new-text: %p" new-text)
  # should cursor position be adjusted
  (def [container-start-line container-start-column
        old-container-end-line old-container-end-column]
    (let [attrs (j/attrs container-zloc)]
      [(attrs :bl) (attrs :bc)
       (attrs :el) (attrs :ec)]))
  (def new-zloc
    (-> (l/ast new-text)
        j/zip-down))
  (def new-container-zloc
    (find-container-for-lc new-zloc
                           container-start-line container-start-column))
  # XXX: should not happen?
  (unless (container? new-container-zloc)
    (eprintf "unexpectedly did not find new container"))
  (eprintf "new-container node: %p" (j/node new-container-zloc))
  (var new-cursor [cursor-l cursor-c])
  (eprintf "new-cursor: %p" new-cursor)
  (let [new-container-span (lc-for-zloc new-container-zloc)]
    (eprintf "new container-span: %p" new-container-span)
    (unless (spans? new-container-span
                    # XXX: is this wrong?
                    [cursor-l cursor-c cursor-l (inc cursor-c)])
      (eprintf "new container didn't span")
      (set new-cursor
           [(get new-container-span 2)
            (dec (get new-container-span 3))])))
  [new-text new-cursor])

(comment

  (def src
    ``
    (defn my-fn
      [x]
      (+ x 1) :a)

    (def a 2)
    ``)

  (eject-forward [3 5] src)
  # =>
  [``
   (defn my-fn
     [x]
     (+ x) 1 :a)

   (def a 2)
   ``
   [3 5]]

  (def src
    ``
    (defn my-fn
      [x]
      (+ x 1) :a :b)

    (def a 2)
    ``)

  (def [new-src [new-cursor-l new-cursor-c]]
    (->> src
         (eject-forward [3 9])))

  (->> new-src
       (eject-forward [new-cursor-l new-cursor-c]))
  # =>
  [``
   (defn my-fn
     [x]
     (+) x 1 :a :b)

   (def a 2)
   ``
   [3 5]]

  )

(defn forward-expr
  [[line column] src]
  (def curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [line column]))
  (unless cursor-zloc
    (eprintf "did not find zloc for: [%p %p]" line column)
    (break nil))
  (eprintf "cursor node: %p" (j/node cursor-zloc))
  (def dest-zloc
    (match (j/node cursor-zloc)
      [:whitespace]
      (j/right-skip-wsc cursor-zloc)
      #
      [:comment]
      (j/right-skip-wsc cursor-zloc)
      #
      cursor-zloc))
  (when dest-zloc
    (eprintf "dest-zloc: %p" (j/node dest-zloc))
    (def {:ec ec :el el} (j/attrs dest-zloc))
    (break [el ec])))

(comment

  (forward-expr [1 8] "(+ 1000 1)")
  # =>
  '(1 10)

  (forward-expr [1 9] "(+ 1111)")
  # =>
  [1 9]

  )

(defn backward-expr
  [[line column] src]
  (def curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [line column]))
  (unless cursor-zloc
    (eprintf "did not find zloc for: [%p %p]" line column)
    (break nil))
  (eprintf "cursor node: %p" (j/node cursor-zloc))
  (def dest-zloc
    (match (j/node cursor-zloc)
      [:whitespace]
      (j/left-skip-wsc cursor-zloc)
      #
      [:comment]
      (j/left-skip-wsc cursor-zloc)
      #
      (let [{:bc bc :bl bl} (j/attrs cursor-zloc)]
        (if (and (= column bc)
                 (= line bl))
          (j/left-skip-wsc cursor-zloc)
          cursor-zloc))))
  (when dest-zloc
    (eprintf "dest-zloc: %p" (j/node dest-zloc))
    (def {:bc bc :bl bl} (j/attrs dest-zloc))
    (break [bl bc])))

(comment

  (backward-expr [1 7] "(+ 1000 1)")
  # =>
  [1 4]

  (backward-expr [1 1] "(+ 1 11)")
  # =>
  nil

  )

(defn delete-forward-expr
  [[line column] src]
    (def curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [line column]))
  (unless cursor-zloc
    (eprintf "did not find zloc for: [%p %p]" line column)
    (break nil))
  (def last-sibling-zloc
    (j/rightmost cursor-zloc))
  (unless last-sibling-zloc
    (eprintf "did not find last sibling")
    (break nil))
  (let [{:ec c :el l} (j/attrs last-sibling-zloc)]
    [l c]))

(comment

  (delete-forward-expr [1 6] "(+ 1 2)")
  # =>
  '(1 7)

  (delete-forward-expr [1 6]
                       ``
                       (+ 1 2
                            3 5
                              7)
                       ``)
  # =>
  '(3 9)

  )

(defn backward-up-expr
  [[line column] src]
  (def curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  (def container-zloc
    (find-container-for-lc curr-zloc line column))
  (unless container-zloc
    (eprintf "did not find container for: [%p %p] (1)" line column)
    (break nil))
  (def {:bc c :bl l}
    (j/attrs container-zloc))
  (eprintf "container span: [%p %p]" l c)
  (when (or (not= line l)
            (not= column c))
    (break [l c]))
  (def outer-container-zloc
    (j/up container-zloc))
  (unless outer-container-zloc
    (eprintf "did not find outer container")
    (break nil))
  (def {:bc oc :bl ol}
    (j/attrs outer-container-zloc))
  (eprintf "outer container span: [%p %p]" ol oc)
  (eprintf "outer container node: %p" (j/node outer-container-zloc))
  (def oc-node-type
    (first (j/node outer-container-zloc)))
  (if (not= :code oc-node-type)
    [ol oc]
    nil))

(comment

  (def src
    "[:a :b :c]")

  (backward-up-expr [1 2] src)
  # =>
  [1 1]

  (backward-up-expr [1 6] src)
  # =>
  [1 1]

  (backward-up-expr [1 1] src)
  # =>
  nil

  (def src
    "[:a [:x :y]]")

  (backward-up-expr [1 6] src)
  # =>
  [1 5]

  (backward-up-expr [1 5] src)
  # =>
  [1 1]

  )

(defn atom?
  ``
  Returns true if `zloc` is an atom node, false otherwise.

  An atom node is one of the following types:

  * number
  * constant
  * buffer
  * string
  * long-string
  * long-buffer
  * keyword
  * symbol
  ``
  [zloc]
  (->> (first (j/node zloc))
       (get {:number true
             :constant true
             :buffer true
             :string true
             :long-string true
             :long-buffer true
             :keyword true
             :symbol true})
       truthy?))

(comment

  (-> (l/ast "8")
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast "true")
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast `@""`)
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast `"hi there"`)
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast "``long string``")
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast "@``long buffer``")
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast "# a comment")
      j/zip-down
      atom?)
  # =>
  false

  (-> (l/ast ":fun")
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast "my-symbol")
      j/zip-down
      atom?)
  # =>
  true

  (-> (l/ast " ")
      j/zip-down
      atom?)
  # =>
  false

  )

(defn forward-atom
  [[line column] src]
  (def curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [line column]))
  (unless cursor-zloc
    (eprintf "did not find zloc for cursor")
    (break nil))
  (var temp-zloc cursor-zloc)
  (eprintf "temp-zloc node: %p" (j/node temp-zloc))
  (var found-target nil)
  # if at an atom already, could be end of search
  (when (atom? temp-zloc)
    (def {:ec ec}
      (j/attrs temp-zloc))
    (when (not= ec column)
      (set found-target true)))
  # search not finished
  (when (not found-target)
    (while (not (j/end? temp-zloc))
      (set temp-zloc
           (j/df-next temp-zloc))
      (def node-type
        (first (j/node temp-zloc)))
      (unless (or (= :whitespace node-type)
                  (= :comment node-type)
                  (container? temp-zloc))
        (set found-target true)
        (break))))
  (when found-target
    (def {:ec ec :el el}
      (j/attrs temp-zloc))
    [el ec]))

(comment

  (forward-atom [1 2] "[:ant :bee]")
  # =>
  [1 6]

  (forward-atom [1 4] "[:a [:b :c]]")
  # =>
  [1 8]

  (forward-atom [1 1] "(a)\n:x")
  # =>
  [1 3]

  (forward-atom [1 2] "(a)\n:x")
  # =>
  [1 3]

  (forward-atom [1 3] "(a)\n:x")
  # =>
  [2 3]

  )

(defn backward-atom
  [[line column] src]
  (def curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [line column]))
  (unless cursor-zloc
    (eprintf "did not find zloc for cursor")
    (break nil))
  (var temp-zloc cursor-zloc)
  (eprintf "temp-zloc node: %p" (j/node temp-zloc))
  (var found-target nil)
  # if at an atom already, could be end of search
  (when (atom? temp-zloc)
    (def {:bc bc}
      (j/attrs temp-zloc))
    (when (not= bc column)
      (set found-target true)))
  # search not finished
  (when (not found-target)
    # XXX: does this work in reverse?
    (while (not (j/end? temp-zloc))
      (set temp-zloc
           (j/df-prev temp-zloc))
      (def node-type
        (first (j/node temp-zloc)))
      (unless (or (= :whitespace node-type)
                  (= :comment node-type)
                  (container? temp-zloc))
        (set found-target true)
        (break))))
    (when found-target
    (def {:bc bc :bl bl}
      (j/attrs temp-zloc))
    [bl bc]))

(comment

  (backward-atom [1 6] "[:ant :bee]")
  # =>
  [1 2]

  (backward-atom [1 6] "[:a [:b :c]]")
  # =>
  [1 2]

  (backward-atom [2 3] ":x\n(a)")
  # =>
  [2 2]

  (backward-atom [2 2] ":x\n(a)")
  # =>
  [1 1]

  (backward-atom [2 1] ":x\n(a)")
  # =>
  [1 1]

  )

(defn forward-down-expr
  [[line column] src]
  (def curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [line column]))
  (unless cursor-zloc
    (eprintf "did not find zloc for cursor")
    (break nil))
  # container has particular handling
  (when (container? cursor-zloc)
    (eprintf "cursor at a container")
    (def {:ec ec :el el}
      (j/attrs cursor-zloc))
    (cond
      # on closing delimiter, don't move
      (and (= column ec)
           (= line el)) # XXX: is this the right condition?
      (break nil)
      # has at least one child node
      (j/has-children? (j/node cursor-zloc))
      (let [first-child-zloc (j/down cursor-zloc)]
        (def {:bc cbc :bl cbl}
          (j/attrs first-child-zloc))
        (break [cbl cbc]))
      # no children, move to closing delimiter
      (break [el ec])))
  # not a container
  (eprintf "cursor not at a container: %p" (j/node cursor-zloc))
  (def forward-zloc
    (j/right-until cursor-zloc
                   |(container? $)))
  (unless forward-zloc
    (eprintf "did not find container node in forward direction")
    (break nil))
  (eprintf "forward-zloc: %p" (j/node forward-zloc))
  (if (j/has-children? (j/node forward-zloc))
    (do
      (def {:bc bc :bl bl}
        (j/attrs (j/down forward-zloc)))
      (break [bl bc]))
    (do
      (def {:ec ec :el el}
        (j/attrs forward-zloc))
      (break [el ec]))))

(comment

  (def src
    "[:a [:b :c]]")

  (forward-down-expr [1 2] src)
  # =>
  [1 6]

  (forward-down-expr [1 1] src)
  # =>
  [1 2]

  (forward-down-expr [1 5] src)
  # =>
  [1 6]

  (forward-down-expr [1 11] src)
  # =>
  nil

  (forward-down-expr [1 12] src)
  # =>
  nil

  (def src
    "[:a :b [:x]]")

  (forward-down-expr [1 2] src)
  # =>
  [1 9]

  )

(defn transpose-exprs-old
  [[cursor-l cursor-c] src]
  (var curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "start node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [cursor-l cursor-c]))
  (unless cursor-zloc
    (eprintf "did not find expr for cursor")
    (break nil))
  (eprintf "cursor node: %p" (j/node cursor-zloc))
  # find current non-whitespace, non-comment expr
  (def right-zloc
    (match (j/node cursor-zloc)
      [:comment]
      (do
        (eprintf "invocation on comment unsupported")
        (break nil))
      #
      [:whitespace]
      (if-let [new-right-zloc (j/right-skip-wsc cursor-zloc)]
        new-right-zloc
        (do
          (eprintf "failed to find non-whitespace, non-comment (1)")
          (break nil)))
      #
      cursor-zloc))
  (eprintf "right node: %p" (j/node right-zloc))
  (def end-attrs
    (j/attrs right-zloc))
  # check if there is a previous non-whitespace, non-comment expr
  (var temp-zloc (j/left cursor-zloc))
  (unless temp-zloc
    (eprintf "did not find expr to the left of the cursor")
    (break nil))
  # how many right moves it takes to get from left-zloc to right-zloc
  (var n-nodes-to-right 0)
  (eprintf "temp-zloc: %p" (j/node temp-zloc))
  (def left-zloc
    (match (j/node temp-zloc)
      [:comment]
      (do
        (eprintf "comment to left of cursor not supported")
        (break nil))
      #
      [:whitespace]
      (if-let [[new-left-zloc n-moved]
               (j/left-skip-wsc-with-count temp-zloc)]
        (do
          (eprintf "skipped over %p nodes" n-moved)
          # moved left once before left-skip-wsc-with-count, thus `inc`
          (+= n-nodes-to-right (inc n-moved))
          new-left-zloc)
        (do
          (eprintf "failed to find non-whitespace, non-comment (2)")
          (break nil)))
      #
      (do
        (eprintf "neither comment nor whitespace")
        # XXX: calculate how many nodes to right, right-zloc is, that
        #      is what n-nodes-to-right should be
        # XXX: hack -- may not be right always
        (set n-nodes-to-right 2)
        temp-zloc)))
  (eprintf "left node: %p" (j/node left-zloc))
  (eprintf "need to remove %p nodes" n-nodes-to-right)
  # swap
  # XXX: doesn't work across newlines properly?
  (set curr-zloc
       (-> left-zloc
           # do the remove-right-n before changing the zipper,
           # otherwise may not work properly
           (j/remove-right-n n-nodes-to-right)
           (j/insert-left (j/node right-zloc))
           (j/insert-left [:whitespace @{} " "])))
  # determine end position of latter expr
  # XXX: not sure if this quite right
  (def new-cursor
    [(end-attrs :el)
     (end-attrs :ec)])
  (eprintf "new-cursor: %p" new-cursor)
  # replacement text
  (def new-text
    (-> curr-zloc
        j/root
        l/code))
  (eprintf "new-text: %p" new-text)
  [new-text new-cursor])

# XXX: try new idea
(defn transpose-exprs
  [[cursor-l cursor-c] src]
  (var curr-zloc
    (-> (l/ast src)
        j/zip-down))
  (eprintf "start node: %p" (j/node curr-zloc))
  (def cursor-zloc
    (find-zloc-for-lc curr-zloc [cursor-l cursor-c]))
  (unless cursor-zloc
    (eprintf "did not find expr for cursor")
    (break nil))
  (eprintf "cursor node: %p" (j/node cursor-zloc))
  # find current non-whitespace, non-comment expr
  (def right-zloc
    (match (j/node cursor-zloc)
      [:comment]
      (do
        (eprintf "invocation on comment unsupported")
        (break nil))
      #
      [:whitespace]
      (if-let [new-right-zloc (j/right-skip-wsc cursor-zloc)]
        new-right-zloc
        (do
          (eprintf "failed to find non-whitespace, non-comment (1)")
          (break nil)))
      #
      cursor-zloc))
  (eprintf "right node: %p" (j/node right-zloc))
  (def end-attrs
    (j/attrs right-zloc))
  # check if there is a previous non-whitespace, non-comment expr
  (var temp-zloc (j/left cursor-zloc))
  (unless temp-zloc
    (eprintf "did not find expr to the left of the cursor")
    (break nil))
  # how many right moves it takes to get from left-zloc to right-zloc
  (var n-nodes-to-right 0)
  (eprintf "temp-zloc: %p" (j/node temp-zloc))
  (def left-zloc
    (match (j/node temp-zloc)
      [:comment]
      (do
        (eprintf "comment to left of cursor not supported")
        (break nil))
      #
      [:whitespace]
      (if-let [[new-left-zloc n-moved]
               (j/left-skip-wsc-with-count temp-zloc)]
        (do
          (eprintf "skipped over %p nodes" n-moved)
          # moved left once before left-skip-wsc-with-count, thus `inc`
          (+= n-nodes-to-right (inc n-moved))
          new-left-zloc)
        (do
          (eprintf "failed to find non-whitespace, non-comment (2)")
          (break nil)))
      #
      (do
        (eprintf "neither comment nor whitespace")
        # XXX: calculate how many nodes to right, right-zloc is, that
        #      is what n-nodes-to-right should be
        # XXX: hack -- may not be right always
        (set n-nodes-to-right 2)
        temp-zloc)))
  (eprintf "left node: %p" (j/node left-zloc))
  (eprintf "need to remove %p nodes" n-nodes-to-right)
  # swap
  # XXX: doesn't work across newlines properly?
  (set curr-zloc
       (-> left-zloc
           # do the remove-right-n before changing the zipper,
           # otherwise may not work properly
           (j/remove-right-n n-nodes-to-right)
           (j/insert-left (j/node right-zloc))
           (j/insert-left [:whitespace @{} " "])))
  # determine end position of latter expr
  # XXX: not sure if this quite right
  (def new-cursor
    [(end-attrs :el)
     (end-attrs :ec)])
  (eprintf "new-cursor: %p" new-cursor)
  # replacement text
  (def new-text
    (-> curr-zloc
        j/root
        l/code))
  (eprintf "new-text: %p" new-text)
  [new-text new-cursor])

(comment

  (transpose-exprs [1 4] "(+ 1 2)")
  # =>
  '("(1 + 2)" (1 5))

  (transpose-exprs [1 6] "(+ 1 2)")
  # =>
  '("(+ 2 1)" (1 7))

  (transpose-exprs [1 11] "(+ (- 2 1) (+ 3 2))")
  # =>
  '("(+ (+ 3 2) (- 2 1))" (1 19))

  # XXX: not working yet
  (transpose-exprs [1 11]
                   ``
                   (+ (- 2 1)
                      (+ 3 2))
                   ``)
  # =>
  [``
   (+ (+ 3 2)
      (- 2 1))
   ``
   [2 11]]

  )
