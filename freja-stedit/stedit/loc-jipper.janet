(import ./zipper :as z)

(defn has-children?
  ``
  Returns true if `node` can have children.
  Returns false if `node` cannot have children.
  ``
  [a-node]
  (when-let [[head] a-node]
    (truthy? (get {:code true
                   :fn true
                   :quasiquote true
                   :quote true
                   :splice true
                   :unquote true
                   :array true
                   :tuple true
                   :bracket-array true
                   :bracket-tuple true
                   :table true
                   :struct true}
                  head))))

(comment

  (has-children?
    [:tuple @{}
     [:symbol @{} "+"] [:whitespace @{} " "]
     [:number @{} "1"] [:whitespace @{} " "]
     [:number @{} "2"]])
  # =>
  true

  (has-children? [:number @{} "8"])
  # =>
  false

  )

(defn zip
  ``
  Returns a zipper location (zloc or z-location) for a tree
  representing Janet code.
  ``
  [tree]
  (defn branch?
    [a-node]
    (truthy? (and (indexed? a-node)
                  (not (empty? a-node))
                  (has-children? a-node))))
  #
  (defn children
    [a-node]
    (if (branch? a-node)
      (slice a-node 2)
      (error "Called `children` on a non-branch node")))
  #
  (defn make-node
    [a-node children]
    [(first a-node) @{} ;children])
  #
  (z/zipper tree
            :branch? branch?
            :children children
            :make-node make-node))

(comment

  (def root-node
    @[:code @{} [:number @{} "8"]])

  (def [the-node the-state]
    (zip root-node))

  the-node
  # =>
  root-node

  (merge {} the-state)
  # =>
  @{}

  )

(defn attrs
  ``
  Return the attributes table for the node of a z-location.  The
  attributes table contains at least bounds of the node by 1-based line
  and column numbers.
  ``
  [zloc]
  (get (z/node zloc) 1))

(comment

  (type (import ./location :as l))
  # =>
  :table

  )

(comment

  (-> (l/ast "(+ 1 3)")
      zip
      z/down
      attrs)
  # =>
  @{:bc 1 :bl 1 :ec 8 :el 1}

  )

(defn zip-down
  ``
  Convenience function that returns a zipper which has
  already had `down` called on it.
  ``
  [tree]
  (-> (zip tree)
      z/down))

(comment

  (-> (l/ast "(+ 1 3)")
      zip-down
      z/node)
  # =>
  '(:tuple @{:bc 1 :bl 1
             :ec 8 :el 1}
           (:symbol @{:bc 2 :bl 1
                      :ec 3 :el 1} "+")
           (:whitespace @{:bc 3 :bl 1
                          :ec 4 :el 1} " ")
           (:number @{:bc 4 :bl 1
                      :ec 5 :el 1} "1")
           (:whitespace @{:bc 5 :bl 1
                          :ec 6 :el 1} " ")
           (:number @{:bc 6 :bl 1
                      :ec 7 :el 1} "3"))

  )

(defn right-until
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [right-sib (z/right zloc)]
    (if (pred right-sib)
      right-sib
      (right-until right-sib pred))))

(comment

  (-> [:code @{}
       [:tuple @{}
        [:comment @{} "# hi there"] [:whitespace @{} "\n"]
        [:symbol @{} "+"] [:whitespace @{} " "]
        [:number @{} "1"] [:whitespace @{} " "]
        [:number @{} "2"]]]
      zip-down
      z/down
      (right-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      z/node)
  # =>
  [:symbol @{} "+"]

  (-> [:code @{}
       [:tuple @{}
        [:keyword @{} ":a"]]]
      zip-down
      z/down
      (right-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true)))
  # =>
  nil

  )

# wsc == whitespace, comment
(defn right-skip-wsc
  ``
  Try to move right from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one right move succeeds, return the z-location
  for the last successful right move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (right-until zloc
               |(match (z/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  #(import ./location :as l)

  (-> (l/ast
        ``
        (# hi there
        + 1 2)
        ``)
      zip-down
      z/down
      right-skip-wsc
      z/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (l/ast "(:a)")
      zip-down
      z/down
      right-skip-wsc)
  # =>
  nil

  )

(defn left-until
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling.
  Otherwise, return nil.
  ``
  [zloc pred]
  (when-let [left-sib (z/left zloc)]
    (if (pred left-sib)
      left-sib
      (left-until left-sib pred))))

(comment

  #(import ./location :as l)

  (-> (l/ast
        ``
        (# hi there
        + 1 2)
        ``)
      zip-down
      z/down
      right-skip-wsc
      right-skip-wsc
      (left-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true))
      z/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> [:code @{}
       [:tuple @{}
        [:keyword @{} ":a"]]]
      zip-down
      z/down
      (left-until |(match (z/node $)
                      [:comment]
                      false
                      #
                      [:whitespace]
                      false
                      #
                      true)))
  # =>
  nil

  )

(defn left-skip-wsc
  ``
  Try to move left from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one left move succeeds, return the z-location
  for the last successful left move destination.  Otherwise,
  return nil.
  ``
  [zloc]
  (left-until zloc
               |(match (z/node $)
                  [:whitespace]
                  false
                  #
                  [:comment]
                  false
                  #
                  true)))

(comment

  #(import ./location :as l)

  (-> (l/ast
        ``
        (# hi there
        + 1 2)
        ``)
      zip-down
      z/down
      right-skip-wsc
      right-skip-wsc
      left-skip-wsc
      z/node)
  # =>
  [:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"]

  (-> (l/ast "(:a)")
      zip-down
      z/down
      left-skip-wsc)
  # =>
  nil

  )

(defn right-until-with-count
  ``
  Try to move right from `zloc`, calling `pred` for each
  right sibling.  If the `pred` call has a truthy result,
  return the corresponding right sibling and the number of
  moves performed.  Otherwise, return nil.
  ``
  [zloc pred]
  (defn helper
    [curr-zloc pred i]
    (when-let [right-sib (z/right curr-zloc)]
      (if (pred right-sib)
        [right-sib (inc i)]
        (helper right-sib pred (inc i)))))
  #
  (helper zloc pred 0))

(comment

  (def [zloc cnt]
    (-> [:code @{}
         [:tuple @{}
          [:comment @{} "# hi there"] [:whitespace @{} "\n"]
          [:symbol @{} "+"] [:whitespace @{} " "]
          [:number @{} "1"] [:whitespace @{} " "]
          [:number @{} "2"]]]
        zip-down
        z/down
        (right-until-with-count |(match (z/node $)
                                   [:comment]
                                   false
                                   #
                                   [:whitespace]
                                   false
                                   #
                                   true))))

  [(z/node zloc) cnt]
  # =>
  [[:symbol @{} "+"] 2]

  (def result
    (-> [:code @{}
         [:tuple @{}
          [:keyword @{} ":a"]]]
        zip-down
        z/down
        (right-until-with-count |(match (z/node $)
                                   [:comment]
                                   false
                                   #
                                   [:whitespace]
                                   false
                                   #
                                   true))))
  # =>
  nil

  )

(defn left-until-with-count
  ``
  Try to move left from `zloc`, calling `pred` for each
  left sibling.  If the `pred` call has a truthy result,
  return the corresponding left sibling and the number of
  moves performed.  Otherwise, return nil.
  ``
  [zloc pred]
  (defn helper
    [curr-zloc pred i]
    (when-let [left-sib (z/left curr-zloc)]
      (if (pred left-sib)
        [left-sib (inc i)]
        (helper left-sib pred (inc i)))))
  #
  (helper zloc pred 0))

(comment

  (def [zloc cnt]
    (-> [:code @{}
         [:tuple @{}
          [:comment @{} "# hi there"] [:whitespace @{} "\n"]
          [:symbol @{} "+"] [:whitespace @{} " "]
          [:number @{} "1"] [:whitespace @{} " "]
          [:number @{} "2"]]]
        zip-down
        z/down
        z/rightmost
        (left-until-with-count |(match (z/node $)
                                  [:comment]
                                  false
                                  #
                                  [:whitespace]
                                  false
                                  #
                                  true))))

  [(z/node zloc) cnt]
  # =>
  [[:number @{} "1"] 2]

  (def result
    (-> [:code @{}
         [:tuple @{}
          [:keyword @{} ":a"]]]
        zip-down
        z/down
        (left-until-with-count |(match (z/node $)
                                  [:comment]
                                  false
                                  #
                                  [:whitespace]
                                  false
                                  #
                                  true))))
  # =>
  nil

  )

(defn right-skip-wsc-with-count
  ``
  Try to move right from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one right move succeeds, return a tuple of the
  z-location for the last successful right move destination
  along with a count of the successful right moves.  Otherwise,
  return nil.
  ``
  [zloc]
  (right-until-with-count zloc
                          |(match (z/node $)
                             [:whitespace]
                             false
                             #
                             [:comment]
                             false
                             #
                             true)))

(comment

  #(import ./location :as l)

  (def [zloc cnt]
    (-> (l/ast
          ``
          (# hi there
          + 1 2)
          ``)
        zip-down
        z/down
        right-skip-wsc-with-count))

  [(z/node zloc) cnt]
  # =>
  [[:symbol @{:bc 1 :bl 2 :ec 2 :el 2} "+"] 2]

  (def result
    (-> (l/ast "(:a)")
        zip-down
        z/down
        right-skip-wsc-with-count))
  # =>
  nil

  )

(defn left-skip-wsc-with-count
  ``
  Try to move left from `zloc`, skipping over whitespace
  and comment nodes.

  When at least one left move succeeds, return a tuple of the
  z-location for the last successful left move destination
  along with a count of the successful left moves.  Otherwise,
  return nil.
  ``
  [zloc]
  (left-until-with-count zloc
                         |(match (z/node $)
                            [:whitespace]
                            false
                            #
                            [:comment]
                            false
                            #
                            true)))

(comment

  #(import ./location :as l)

  (def [zloc cnt]
    (-> (l/ast
          ``
          (# hi there
          + 1 2)
          ``)
        zip-down
        z/down
        z/rightmost
        left-skip-wsc-with-count))

  [(z/node zloc) cnt]
  # =>
  [[:number @{:bc 3 :bl 2 :ec 4 :el 2} "1"] 2]

  (def result
    (-> (l/ast "(:a)")
        zip-down
        z/down
        left-skip-wsc-with-count))
  # =>
  nil

  )

(defn remove-right-n
  [zloc n]
  ``
  Verify that there are `n` right siblings of `zloc` and if there
  are, remove them.  The returned z-location should be at the same
  node as that of `zloc`.

  If there are less than `n` right siblings, return nil.
  ``
  (var curr-zloc zloc)
  #
  (unless (z/right curr-zloc)
    (eprintf "no right sibling")
    (break nil))
  (def target-node (z/node zloc))
  (eprintf "target-node: %p" target-node)
  # find depth for final repositioning
  (var d 0)
  # go right and then depth-first backwards to get to the
  # z-location that one would end up on if the right node
  # was removed
  (var temp-zloc
    (-> curr-zloc
        z/right
        z/df-prev))
  # XXX: comparison not working because `up` leads to location info
  #      loss...sometimes :(
  (while (not (deep= target-node (z/node temp-zloc)))
    (eprintf "comparing node: %p" (z/node temp-zloc))
    (++ d)
    (set temp-zloc
         (z/up temp-zloc)))
  (eprintf "climbed %p nodes" d)
  # check that there are n right siblings and modify them to be
  # non-containers, left-to-right
  (var found-n-sibs true)
  (for i 0 n
    (if-let [right-sib (z/right curr-zloc)]
      (set curr-zloc
           # prepare for removal
           (z/replace right-sib [:whitespace @{} " "]))
      (do
        (set found-n-sibs false)
        (break))))
  (unless found-n-sibs
    (eprintf "did not find %p right siblings" n)
    (break nil))
  # remove the n right siblings, right-to-left
  (for i 0 n
    (set curr-zloc
         (z/remove curr-zloc)))
  # climb up an appropriate amount as determined earlier so
  # that the returned z-location has the same node as the original
  # z-location
  (for j 0 d
    (eprintf "climb number: %p" j)
    (set curr-zloc
         (z/up curr-zloc)))
  curr-zloc)

(comment

  #(import ./location :as l)

  (-> (l/ast "[:a :b :c]")
      zip-down
      z/down
      (remove-right-n (+ 2 2)) # includes whitespace
      z/root
      l/code)
  # =>
  "[:a]"

  (-> (l/ast "[[:a [:x :y]] :b :c]")
      zip-down
      z/down
      (remove-right-n (+ 2 2)) # includes whitespace
      z/root
      l/code)
  # =>
  "[[:a [:x :y]]]"

  (-> (l/ast "(+ (- 2 1) (+ 3 2))")
      zip-down
      z/down
      right-skip-wsc
      (remove-right-n 2)
      z/root
      l/code)
  # =>
  "(+ (- 2 1))"

  )
