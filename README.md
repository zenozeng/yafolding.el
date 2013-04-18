# yafolding - Yet another folding extension for the Emacs editor

Folding code blocks based on indentation.  

## ScreenShot
![Alt text](https://raw.github.com/zenozeng/yafolding.el/master/psc.png)


## Config Example

```emacs-lisp

(define-key global-map (kbd "C-'") 'yafolding)
;;(define-key global-map (kbd "C-c C-f") 'yafolding-toggle-all)
(define-key global-map (kbd "C-c C-f") 'yafolding-toggle-all-by-current-level)
;;(add-hook 'indent-buffer-before-hook
;;	  (lambda ()
;;	    (yafolding-temp-toggle nil)))
;;(add-hook 'indent-buffer-after-hook
;;	  (lambda ()
;;	    (yafolding-temp-toggle t)))
```


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


