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
    (get (j/node zloc) 1))
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
                        [6 6])
      j/node)
  # =>
  '(:symbol @{:bc 6 :bl 6 :ec 7 :el 6} "a")

  )

(defn find-absorbee
  [container-zloc]
  # look to the right, skipping whitespace if comment is found, abort
  # (not skip)
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

  (-> (l/ast ":a")
      j/zip-down
      container?)
  # =>
  false

  )

(defn find-container-for-lc
  [zloc l c]
  (def a-zloc
    (find-zloc-for-lc zloc [l c]))
  (unless a-zloc
    (eprintf "did not find zloc for: [%p %p]" l c)
    (break nil))
  #
  (j/up a-zloc))

(defn absorb-right
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
  # find what to absorb
  (def absorbee-pair
    (find-absorbee container-zloc))
  (unless absorbee-pair
    (eprintf "did not find absorbee (1)")
    (break nil))
  (def absorbee-node
    (j/node (get absorbee-pair 0)))
  # absorb absorbee
  (set curr-zloc
       (-> container-zloc
           (j/append-child [:whitespace @{} " "])
           (j/append-child absorbee-node)))
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
  (set curr-zloc absorbee-zloc)
  (eprintf "node: %p" (j/node curr-zloc))
  (eprintf "cnt: %p" cnt)
  # replace each removal candidate with a non-container so that
  # removal leads to a straight-forward "destination"
  (for i 0 cnt
    (set curr-zloc
         (-> (j/replace curr-zloc [:whitespace @{} " "])
             j/remove)))
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

  (absorb-right [3 6] src)
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
       (absorb-right [3 6])
       (absorb-right [3 6]))
  # =>
  ``
  (defn my-fn
    [x]
    (+ x 1 :a :b))

  (def a 2)
  ``

  )
