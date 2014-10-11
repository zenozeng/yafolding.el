# yafolding - Yet another folding extension for Emacs

Folding code blocks based on indentation.

## ScreenShot
![PrtSc](https://raw.github.com/zenozeng/yafolding.el/master/psc.png)

## Config Example

### Default Keymap

```emacs-lisp
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
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

## Known Issue

### data loss on delete-trailing-whitespace

use this instated:

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
