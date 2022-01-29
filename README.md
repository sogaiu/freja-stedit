# freja-stedit

Structural editing and navigation for the freja editor.

## Prerequisites

The [freja editor](https://github.com/saikyun/freja).

## Setup

0. Clone this repository somewhere and cd to the resulting directory
1. Start freja with: `freja ./freja-stedit/defaults.janet`
2. `Control+L` to load the file

## Commands

### Navigation

* `forward-expr` - `ctrl-alt-f`

* `backward-expr` - `ctrl-alt-b`

* `forward-down-expr` - `ctrl-alt-d`

* `backward-up-expr` - `ctrl-alt-u`

* `forward-atom` - `ctrl-alt-shift-f`

* `backward-atom` - `ctrl-alt-shift-b`

* `forward-end-of-top-level` - `ctrl-alt-e`

* `backward-start-of-top-level` - `ctrl-alt-a`

### Editing

* `delete-forward-expr` - `ctrl-alt-k`

* `select-forward-expr` - `ctrl-alt-shift-k`

* `eject-forward` - `ctrl-shift-]` or `ctrl-}`

* `absorb-forward` - `ctrl-shift-0` or `ctrl-)`

## Naming

I didn't like the "traditional" name for pushing out the last element
of a container, nor the name for pulling in the following expression
into a container, so I chose different ones.

Similar for the word that's typically used for deletion.

