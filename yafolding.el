;;; yafolding.el --- Yet another folding extension for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017 Zeno Zeng

;; Author: Zeno Zeng <zenoofzeng@gmail.com>
;; keywords: folding
;; Time-stamp: <2026-11-04 13:37:36 EST, updated by Pierre Rouleau>
;; Version: 0.4.0


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

(declare-function discover-add-context-menu "discover")

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
  "Text to show in place of a folded block."
  :tag "Ellipsis"
  :type 'string
  :group 'yafolding
  )

(defcustom yafolding-show-fringe-marks t
  "Show fold markers in the fringe?"
  :tag "Show fringe marks?"
  :type 'boolean
  :group 'yafolding)

(defvar-local yafolding--folded-p nil
  "Non-nil if yafolding overlays existed at the last state update.
The buffer-local binding survives normal major-mode changes so that
whitespace cleanup can detect folds that survive those changes.
Do not set this variable directly.")

;; Fold overlays can survive a major-mode change.
;; Preserve the local binding used by the whitespace-cleanup guard.
(put 'yafolding--folded-p 'permanent-local t)

(defun yafolding-get-overlays (beg end)
  "Get all overlays between BEG and END."
  (delq nil
        (mapcar (lambda (overlay)
                  (and (member "yafolding" (overlay-properties overlay))
                       overlay))
                (overlays-in beg end))))

(defun yafolding-should-ignore-current-line-p ()
  "Return if should ignore current line."
  (string-match-p "^[ \t]*$"
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))

(defun yafolding-get-indent-level ()
  "Get the indent level of current line."
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

(defun yafolding--update-folded-p ()
  "Refresh and return the folding state of the entire current buffer."
  (setq yafolding--folded-p
        (save-restriction
          (widen)
          (and (yafolding-get-overlays (point-min) (point-max)) t))))

(defun yafolding-show-region (beg end)
  "Delete all yafolding overlays between BEG and END."
  (prog1
      (mapcar #'delete-overlay (yafolding-get-overlays beg end))
    (yafolding--update-folded-p)))

(defun yafolding-show-all ()
  "Delete all yafolding overlays."
  (interactive)
  (save-restriction
    (widen)
    (yafolding-show-region (point-min) (point-max))))

;; --
;; Support for delete-trailing-whitespace
;; --------------------------------------
;;
;; The goal is compatibility in both directions:
;;
;; Folding must not change which text `delete-trailing-whitespace' deletes.
;; Automatic whitespace cleanup should not remove folds unnecessarily.
;; After cleanup, yafolding commands must continue to work.
;;
;; - If yafolding has never touched the current buffer, call the original
;;   function directly. Perform only the local-binding check.
;; - If the buffer has yafolding overlays, save the relevant overlay
;;   properties.
;; - Temporarily disable the overlays’ intangible behavior and their
;;   fold-removal modification hooks.
;; - Call the original delete-trailing-whitespace with its original arguments
;;   and narrowing.
;; - Use unwind-protect to restore the saved properties, including after an
;;   error or quit.
;; - Refresh the folding-state variable after cleanup.

(defun yafolding--around-delete-trailing-whitespace (original-fct &rest args)
  "Call ORIGINAL-FCT with ARGS while preserving current-buffer folds.
Temporarily disable each yafolding overlay's `intangible' property
and modification hooks.  Restore those properties even if cleanup
exits abnormally.  Overlay bounds follow text deletions normally.
If cleanup deletes all text in a fold, its overlay can evaporate."
  (if (not (local-variable-p 'yafolding--folded-p))
      (apply original-fct args)
    ;; yafolding commands were executed in current buffer.
    (let* ((buffer (current-buffer))
           (saved-ovrls (save-restriction
                          (widen)
                          (mapcar
                           (lambda (overlay)
                             (list overlay
                                   (overlay-get overlay 'intangible)
                                   (overlay-get overlay 'modification-hooks)))
                           (yafolding-get-overlays (point-min) (point-max))))))
      (unwind-protect
          (progn
            ;; Keep visibility and overlay bounds unchanged here.
            ;; Disable only the saved-ovrls yafolding overlays' properties.
            (dolist (state saved-ovrls)
              (overlay-put (car state) 'intangible nil)
              (overlay-put (car state) 'modification-hooks nil))
            (apply original-fct args))
        ;; Restore properties without recreating deleted overlays.
        (dolist (state saved-ovrls)
          (overlay-put (car state) 'intangible (nth 1 state))
          (overlay-put (car state) 'modification-hooks (nth 2 state)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (yafolding--update-folded-p)))))))

(advice-add 'delete-trailing-whitespace :around
            #'yafolding--around-delete-trailing-whitespace)

(defun yafolding-hide-all (&optional indent-level)
  "Hide all elements based on INDENT-LEVEL."
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (save-excursion
    (goto-char (point-min))
    (while (< (line-number-at-pos)
              (line-number-at-pos (point-max)))
      (if (and (= (yafolding-get-indent-level) indent-level)
               (not (yafolding-should-ignore-current-line-p)))
          (yafolding-hide-element))
      (forward-line 1))))

(defun yafolding-toggle-all (&optional indent-level)
  "Toggle folding of the entire file.

If given, toggle all entries that start at INDENT-LEVEL."
  (interactive)
  (unless indent-level
    (setq indent-level (yafolding-get-indent-level)))
  (if (yafolding-get-overlays (point-min) (point-max))
      (yafolding-show-all)
    (yafolding-hide-all indent-level)))

(defun yafolding-ellipsis ()
  "Return propertized ellipsis content."
  (concat " "
          (propertize yafolding-ellipsis-content 'face 'yafolding-ellipsis-face)
          " "))

(defun yafolding-hide-region (beg end)
  "Hide region between BEG and END."
  (when (> end beg)
    ;; Remove overlapping folds without refreshing whole-buffer state.
    ;; Creating the replacement fold below sets `yafolding--folded-p'.
    (mapc #'delete-overlay
          (yafolding-get-overlays beg end))
    (let ((before-string
           (concat
            (when yafolding-show-fringe-marks
              (propertize " " 'display '(left-fringe right-triangle)))
            (yafolding-ellipsis)))
          (new-overlay (make-overlay beg end)))
      (overlay-put new-overlay 'invisible t)
      (overlay-put new-overlay 'intangible t)
      (overlay-put new-overlay 'evaporate t)
      (overlay-put new-overlay 'modification-hooks
                   (list (lambda (overlay &optional _a _b _c _d)
                           (delete-overlay overlay)
                           (yafolding--update-folded-p))))
      (overlay-put new-overlay 'before-string before-string)
      (overlay-put new-overlay 'category "yafolding")
      (setq yafolding--folded-p t))))


(defun yafolding--current-indentation ()
  "Determine indentation in (possibly invisible) current line."
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun yafolding-debug ()
  "Show yafolding information of the current position."
  (interactive)
  (message "indentation: %d, indent level: %d, ingore current line: %s, element-region: %d - %d, (L%d - L%d)"
           (yafolding--current-indentation)
           (yafolding-get-indent-level)
           (yafolding-should-ignore-current-line-p)
           (car (yafolding-get-element-region))
           (car (cdr (yafolding-get-element-region)))
           (line-number-at-pos (car (yafolding-get-element-region)))
           (line-number-at-pos (car (cdr (yafolding-get-element-region))))))

(defun yafolding--next-line ()
  "Move cursor down 1 line over the same column, like `next-line'."
  (let ((target-col (current-column)))
  (when (= 0 (forward-line 1))
    (move-to-column target-col))))

(defun yafolding-get-element-region ()
  "Get \\='(beg end) of current element."
  (let ((beg (line-end-position))
        (end (line-end-position))
        (indentation (current-indentation)))
    (save-excursion
      (yafolding--next-line)
      (while (and (< (line-number-at-pos) (line-number-at-pos (point-max)))
                  (or (> (current-indentation) indentation)
                      (yafolding-should-ignore-current-line-p)))
        (unless (yafolding-should-ignore-current-line-p)
          (setq end (line-end-position)))
        (yafolding--next-line)))
    (list beg end)))

(defun yafolding-hide-element ()
  "Hide current element."
  (interactive)
  (let ((region (yafolding-get-element-region)))
    (yafolding-hide-region (car region)
                           (cadr region))))

(defun yafolding-show-element ()
  "Show current element."
  (interactive)
  (yafolding-show-region (line-beginning-position)
                         (+ 1 (line-end-position))))

(defun yafolding-toggle-element ()
  "Toggle current element."
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

(defun yafolding-go-parent-element ()
  "Go back to parent element."
  (interactive)
  (re-search-backward (concat "^.\\{,"
                              (number-to-string (- (current-indentation) 1))
                              "\\}[^ \t]+")))

(defun yafolding-hide-parent-element ()
  "Hide the parent element."
  (interactive)
  (ignore-errors
    (yafolding-go-parent-element)
    (yafolding-hide-element)))

;; For this feature, you need to install discover.el
;; https://www.github.com/mickeynp/discover.el
(when (boundp 'discover-add-context-menu)
  (discover-add-context-menu
   :context-menu '(yafolding
		   (description "folding based on indentation")
		   (actions
		    ("yafolding"
		     ("h" "hide element" yafolding-hide-element)
		     ("s" "show element" yafolding-show-element)
		     ("t" "toggle element" yafolding-toggle-element)
		     ("H" "hide all" yafolding-hide-all)
		     ("S" "show all" yafolding-show-all)
		     ("T" "toggle all" yafolding-toggle-all)
		     ("p" "go parent element" yafolding-go-parent-element)
		     ("P" "hide parent element" yafolding-hide-parent-element)
		     ("m" "mode" yafolding-mode)))))

  (defalias 'yafolding-discover 'makey-key-mode-popup-yafolding))

;;;###autoload
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))

;;;###autoload
(define-minor-mode yafolding-mode
  "Toggle yafolding mode."
  :keymap yafolding-mode-map)

(provide 'yafolding)
;;; yafolding.el ends here
