# Yggdrasil

An Emacs package for visualizing [Newick format](https://en.wikipedia.org/wiki/Newick_format) phylogenetic trees. Place your cursor inside a Newick string, invoke `yggdrasil-visualize`, and get an interactive ASCII tree in a child frame with live cursor tracking.

```
          ┌──────Human                          ┌──┼──┐
  ┌───────┤
  │       └──────Chimp                         ┌┼─┐  ┌┼─┐
──┤                                 or         A  B  C  D
  │       ┌──────Mouse
  └───────┤                               (left-to-right)    (top-down)
          └──────Rat
```

## Requirements

- Emacs 26.1 or later
- GUI Emacs for child frame display (falls back to a split window in terminal)

## Installation

### Manual

Clone or download this repository, then add to your Emacs config:

```elisp
(add-to-list 'load-path "/path/to/Yggdrasil/")
(require 'yggdrasil)
```

### Lazy loading

Only load the package when you first call the command:

```elisp
(add-to-list 'load-path "/path/to/Yggdrasil/")
(autoload 'yggdrasil-visualize "yggdrasil" nil t)
```

### use-package

```elisp
(use-package yggdrasil
  :load-path "/path/to/Yggdrasil/"
  :commands yggdrasil-visualize)
```

## Usage

1. Open a buffer containing a Newick tree string, e.g.:

   ```
   ((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6);
   ```

2. Place your cursor anywhere inside the string.

3. Run `M-x yggdrasil-visualize`.

A child frame pops up showing the ASCII tree. The node under your cursor is highlighted. As you move the cursor through the Newick string, the highlight updates in real time.

## Keybindings

While the tree is displayed, these keys are active in the source buffer:

| Key | Command                       | Description                                      |
|-----|-------------------------------|--------------------------------------------------|
| `q` | `yggdrasil-dismiss`           | Close the tree frame                             |
| `t` | `yggdrasil-toggle-lengths`    | Toggle proportional branch lengths               |
| `r` | `yggdrasil-rotate`            | Toggle between top-down and left-to-right layout |

## Customization

All options are under `M-x customize-group RET yggdrasil`:

| Variable                         | Default | Description                                              |
|----------------------------------|---------|----------------------------------------------------------|
| `yggdrasil-auto-close`           | `nil`   | Auto-close the tree when cursor leaves the Newick string |
| `yggdrasil-show-branch-lengths`  | `nil`   | Use proportional spacing based on branch lengths         |
| `yggdrasil-frame-parameters`     | ...     | Extra parameters for the child frame                     |

The highlight face can be customized with `M-x customize-face RET yggdrasil-highlight`. It inherits from `success` so it adapts to your theme automatically.

## Newick Format

Yggdrasil handles standard Newick syntax:

- Parenthesized trees: `((A,B),(C,D));`
- Branch lengths: `((A:0.1,B:0.2):0.3,C:0.4);`
- Unnamed internal nodes: `((A,B),(C,D));`
- Bare labels: `A;`
- Nested to any depth

## License

GPL-3.0-or-later
