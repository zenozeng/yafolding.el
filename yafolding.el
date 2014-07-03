;;; yafolding.el --- Yet another folding extension for Emacs

;; Copyright (C) 2013-2014  Zeno Zeng

;; Author: Zeno Zeng <zenoofzeng@gmail.com>
;; keywords: folding
;; Time-stamp: <2014-07-03 15:12:09 Zeno Zeng>
;; Version: 0.1.4


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Folding code blocks based on indentation

;;; Code:

(defgroup yafolding nil
  "Fold code blocks based on indentation level"
  :prefix "yafolding-"
  :link '(url-link :tag "yafolding on github" "https://github.com/zenozeng/yafolding.el")
  :group 'applications)

(defface yafolding-ellipsis-face
  '()
  "Face for folded blocks"
  :group 'yafolding)

(defcustom yafolding-ellipsis-content "..."
  "text to show in place of a folded block"
  :tag "Ellipsis"
  :type 'string
  :group 'yafolding
  )

(defcustom yafolding-show-fringe-marks t
  "Show fold markers in the fringe?"
  :tag "Show fringe marks?"
  :type 'boolean
  :group 'yafolding)

(defun yafolding-get-overlays (beg end)
  "Get all overlays between beg and end"
  (delq nil
        (mapcar (lambda (overlay)
                  (and (member "yafolding" (overlay-properties overlay))
                       overlay))
                (overlays-in beg end))))

(defun yafolding-should-ignore-current-line-p ()
  "Return if should ignore current line"
  (string-match-p "^[ \t]*$"
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))

(defun yafolding-get-indent-level ()
  "Get the indent level of current line"
  (interactive)
  (if (and (yafolding-should-ignore-current-line-p)
           (< (line-number-at-pos) (line-number-at-pos (point-max))))
      (save-excursion
        (forward-line 1)
        (yafolding-get-indent-level))
      (let ((indent-level 0)
            (last-indentation (current-indentation)))
        (save-excursion
          (while (and (> (current-indentation) 0)
                      (> (line-number-at-pos) 1))
            (forward-line -1)
            (when (< (current-indentation) last-indentation)
              (setq last-indentation (current-indentation))
              (setq indent-level (+ 1 indent-level)))))
        indent-level)))

(defun yafolding-show-region (beg end)
  "Delete all yafolding overlays between beg and end"
  (mapcar 'delete-overlay (yafolding-get-overlays beg end)))

(defun yafolding-show-all ()
  "Delete all yafolding overlays"
  (interactive)
  (yafolding-show-region (point-min) (point-max)))

(defun yafolding-hide-all (&optional indent-level)
  "Hide all elements based on indent-level"
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (save-excursion
    (goto-char (point-min))
    (while (< (line-number-at-pos)
            (line-number-at-pos (point-max)))
    (if (= (yafolding-get-indent-level) indent-level)
        (yafolding-hide-element))
    (forward-line 1))))

(defun yafolding-toggle-all (&optional indent-level)
  "Toggle folding of the entire file"
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (if (yafolding-get-overlays (point-min) (point-max))
      (yafolding-show-all)
    (yafolding-hide-all indent-level)))

(defun yafolding-ellipsis ()
  (concat " "
          (propertize yafolding-ellipsis-content 'face 'yafolding-ellipsis-face)
          " "))

(defun yafolding-hide-region (beg end)
  "Hide region"
  (when (> end beg)
      (yafolding-show-region beg end)
      (let ((before-string
             (concat
              (when yafolding-show-fringe-marks
                (propertize " " 'display '(left-fringe right-triangle)))
              (yafolding-ellipsis)))
            (new-overlay (make-overlay beg end)))
        (overlay-put new-overlay 'invisible t)
        (overlay-put new-overlay 'intangible t)
        (overlay-put new-overlay 'modification-hooks
                     (list (lambda (overlay &optional a b c d)
                             (delete-overlay overlay))))
        (overlay-put new-overlay 'before-string before-string)
        (overlay-put new-overlay 'category "yafolding"))))

(defun yafolding-get-element-region ()
  "Get '(beg end) of current element"
  (let ((beg (line-end-position))
        (end (line-end-position))
        (indentation (current-indentation)))
    (save-excursion
      (forward-line)
      (while (and (< (line-number-at-pos) (line-number-at-pos (point-max)))
                  (or (> (current-indentation) indentation)
                      (yafolding-should-ignore-current-line-p)))
        (unless (yafolding-should-ignore-current-line-p)
          (setq end (line-end-position)))
        (forward-line 1)))
    (list beg end)))

(defun yafolding-hide-element ()
  "Hide current element"
  (interactive)
  (let ((region (yafolding-get-element-region)))
    (yafolding-hide-region (car region)
                           (cadr region))))

(defun yafolding-show-element ()
  "Show current element"
  (interactive)
  (yafolding-show-region (line-beginning-position)
                         (+ 1 (line-end-position))))

(defun yafolding-toggle-element ()
  "Toggle current element"
  (interactive)
  (if (yafolding-get-overlays (line-beginning-position)
                              (+ 1 (line-end-position)))
      (yafolding-show-element)
    (yafolding-hide-element)))

(add-hook 'isearch-mode-hook
          (lambda() (mapcar (lambda (overlay)
                          (overlay-put overlay 'invisible nil))
                        (yafolding-get-overlays (point-min) (point-max)))))

(add-hook 'isearch-mode-end-hook
          (lambda() (mapcar (lambda (overlay)
                          (overlay-put overlay 'invisible t))
                        (yafolding-get-overlays (point-min) (point-max)))))

;;;###autoload
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))

;;;###autoload
(define-minor-mode yafolding-mode
  "Toggle yafolding mode."
  :keymap yafolding-mode-map)

(provide 'yafolding)
;;; yafolding.el ends here
