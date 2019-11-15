# yafolding - Yet another folding extension for Emacs

[![CircleCI](https://circleci.com/gh/zenozeng/yafolding.el.svg?style=svg)](https://circleci.com/gh/zenozeng/yafolding.el)

Folding code blocks based on indentation.

## ScreenShot
![PrtSc](https://raw.github.com/zenozeng/yafolding.el/master/psc.png)

## Config Example

### Default Keymap

```emacs-lisp
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all) ;; beware, that closes the session on some linux.
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))
```

### Hook into prog-mode-hook

```emacs-lisp
(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))
```

### Modify keybindings

```
(require 'yafolding)
(define-key yafolding-mode-map (kbd "<C-S-return>") nil)
(define-key yafolding-mode-map (kbd "<C-M-return>") nil)
(define-key yafolding-mode-map (kbd "<C-return>") nil)
(define-key yafolding-mode-map (kbd "C-c <C-M-return>") 'yafolding-toggle-all)
(define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-hide-parent-element)
(define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)
```

### Discover commands with a context menu

The [hydra](https://github.com/abo-abo/hydra/) package allows to
display the available commands at once in a magit-like context menu,
and to choose one with a keystroke.

You can copy the following snippet in your init file and call it with
`M-x yafolding-hydra/body`.

```lisp
(defhydra yafolding-hydra (:color blue :columns 3)
  "Fold code based on indentation levels."
  ("t" yafolding-toggle-element "toggle element")
  ("s" yafolding-show-element "show element")
  ("h" yafolding-hide-element "hide element")
  ("T" yafolding-toggle-all "toggle all")
  ("S" yafolding-show-all "show all")
  ("H" yafolding-hide-all "hide all")
  ("p" yafolding-hide-parent-element "hide parent element")
  ("i" yafolding-get-indent-level "get indent level")
  ("g" yafolding-go-parent-element "go parent element"))
```

To give it a keybinding:

    (global-set-key (kbd "C-c f") 'yafolding-hydra/body)

## Known Issue

### data loss on delete-trailing-whitespace

use this instead:

```emacs-lisp
(lambda ()
    (yafolding-show-all)
    (delete-trailing-whitespace))
```

see also: https://github.com/zenozeng/yafolding.el/issues/13

## Licensing

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Debug

- [Using Edebug](https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Edebug.html#Using-Edebug)

## Testing

For testing, use [ecukes](https://github.com/ecukes/ecukes), installed
by [Cask](https://github.com/cask/cask).  The tests are in the
[features](./features/) subdirectory, test data is in the
[data](./test/data) dir.

### Run tests

```bash
cask install
cask exec ecukes --no-win
```
