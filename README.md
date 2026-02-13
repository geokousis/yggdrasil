# Yggdrasil

Yggdrasil is an Emacs package for visualizing [Newick format](https://en.wikipedia.org/wiki/Newick_format) trees as interactive ASCII diagrams.

Place point inside a Newick string and run `M-x yggdrasil-visualize`.

## Features

- Detects Newick around point and renders it live.
- Two layouts: top-down and left-to-right.
- Optional proportional branch-length spacing.
- Point/region-aware highlighting:
  - No region: highlights node at point.
  - Active region: highlights all nodes touched by selected text.
- Jump from rendered tree back to source node location (mouse or keyboard).
- GUI child-frame display with automatic fallback to a normal window.

## Requirements

- Emacs 26.1+

## Installation

### Manual

```elisp
(add-to-list 'load-path "/path/to/yggdrasil/")
(require 'yggdrasil)
```

### Lazy load

```elisp
(add-to-list 'load-path "/path/to/yggdrasil/")
(autoload 'yggdrasil-visualize "yggdrasil" nil t)
```

### use-package

```elisp
(use-package yggdrasil
  :load-path "/path/to/yggdrasil/"
  :commands yggdrasil-visualize)
```

## Usage

1. Open a buffer containing a Newick tree, for example:

```text
((Human:0.1,Chimp:0.2):0.3,(Mouse:0.4,Rat:0.5):0.6)Root;
```

2. Put point inside that string.
3. Run `M-x yggdrasil-visualize`.

## Rendered Output (Actual)

The snapshots below are real output from current `yggdrasil.el` for the example tree above.

### Top-down

```text
           Root
      +------+------+
      |             |
      |             |
   +--+---+      +--+--+
   |      |      |     |
 Human  Chimp  Mouse  Rat
```

### Left-to-right

```text

            +------Human
            |
        +---+
        |   |
        |   +------Chimp
        |
   Root-+
        |
        |   +------Mouse
        |   |
        +---+
            |
            +------Rat
```

## Controls

### In source buffer (while visualization is active)

| Key | Command | Action |
|---|---|---|
| `q` | `yggdrasil-dismiss` | Close visualization |
| `t` | `yggdrasil-toggle-lengths` | Toggle proportional branch lengths |
| `n` | `yggdrasil-toggle-node-numbers` | Toggle node numbers in labels |
| `r` | `yggdrasil-rotate` | Switch orientation |

### In `yggdrasil` render buffer

| Key/Mouse | Command | Action |
|---|---|---|
| `RET` / `<return>` / `C-m` | `yggdrasil-visit-source` | Jump to node location in `.nwk` source |
| `f` | `yggdrasil-visit-source` | Same jump action |
| `mouse-1` | `yggdrasil-visit-source-mouse` | Click node label/anchor to jump |

## Customization

`M-x customize-group RET yggdrasil`

| Variable | Default | Description |
|---|---|---|
| `yggdrasil-auto-close` | `nil` | Close visualization when point leaves Newick bounds |
| `yggdrasil-show-branch-lengths` | `nil` | Scale edges by branch length |
| `yggdrasil-show-node-numbers` | `nil` | Show node numbers as `Label[ID]` / `[ID]` |
| `yggdrasil-display-method` | `auto` | `auto`, `child-frame`, or `window` |
| `yggdrasil-frame-parameters` | alist | Extra child-frame parameters |

Face:

- `M-x customize-face RET yggdrasil-highlight`

## Notes

- In terminal Emacs (`-nw`), mouse support depends on terminal settings; keyboard jump (`RET`) always works.
- Unnamed internal nodes still have a clickable 1-character anchor.

## License

GPL-3.0-or-later
