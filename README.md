# freja-stedit

Structural editing and navigation for the freja editor.

## Prerequisites

The [freja editor](https://github.com/saikyun/freja).

## Setup

0. Clone this repository somewhere and cd to the resulting directory
1. Start freja with: `freja ./freja-stedit/freja-stedit.janet`
2. `Control+L` to load the file

## Commands

* `forward-expr` - `ctrl-alt-f`

* `backward-expr` - `ctrl-alt-b`

* `backward-up-expr` - `ctrl-alt-u`

* `delete-forward-expr` - `ctrl-alt-k`

* `eject-forward` - `ctrl-shift-]` or `ctrl-}`

* `absorb-forward` - `ctrl-shift-0` or `ctrl-)`

## Naming

I didn't like the "traditional" name for pushing out the last element
of a container, nor the name for pulling in the following expression
into a container, so I chose different ones.

Similar for the word that's typically used for deletion.

