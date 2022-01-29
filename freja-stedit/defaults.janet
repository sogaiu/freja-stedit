(import freja/default-hotkeys :as dh)

(import ./freja-stedit :as fse)

(dh/set-key dh/gb-binds
            # XXX: this is control right paren
            [:control :shift :0]
            (comp dh/reset-blink fse/absorb-forward))

(dh/set-key dh/gb-binds
            # XXX: this is control right curly brace
            [:control :shift :right-bracket]
            (comp dh/reset-blink fse/eject-forward))

(dh/set-key dh/gb-binds
            [:control :alt :f]
            (comp dh/reset-blink fse/forward-expr))

(dh/set-key dh/gb-binds
            [:control :alt :b]
            (comp dh/reset-blink fse/backward-expr))

(dh/set-key dh/gb-binds
            [:control :alt :shift :f]
            (comp dh/reset-blink fse/forward-atom))

(dh/set-key dh/gb-binds
            [:control :alt :shift :b]
            (comp dh/reset-blink fse/backward-atom))

(dh/set-key dh/gb-binds
            [:control :alt :d]
            (comp dh/reset-blink fse/forward-down-expr))

(dh/set-key dh/gb-binds
            [:control :alt :u]
            (comp dh/reset-blink fse/backward-up-expr))

(dh/set-key dh/gb-binds
            [:control :alt :a]
            (comp dh/reset-blink fse/backward-start-of-top-level))

(dh/set-key dh/gb-binds
            [:control :alt :e]
            (comp dh/reset-blink fse/forward-end-of-top-level))

(dh/set-key dh/gb-binds
            [:control :alt :k]
            (comp dh/reset-blink fse/delete-forward-expr))

(dh/set-key dh/gb-binds
            [:control :alt :shift :k]
            (comp dh/reset-blink fse/select-forward-expr))

