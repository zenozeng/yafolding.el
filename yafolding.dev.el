;;; yafolding.el --- Yet another folding extension for Emacs

;; Copyright (C) 2013  Zeno Zeng

;; Author: Zeno Zeng <zenoes@qq.com>
;; keywords:
;; Time-stamp: <2013-05-23 20:44:50 Zeno Zeng>


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

(defun yafolding-get-column()
  "Get the column of indentation"
  (back-to-indentation)
  (current-column))

(defun yafolding-line-string-match-p(regexp)
  "Test if current line match the regexp"
  (string-match-p regexp
		  (buffer-substring-no-properties
		   (line-beginning-position)
		   (line-end-position))))

(defun yafolding-my-next-line()
  "Go to next logical line and skip overlay at the same time"
  (let ((line (line-number-at-pos (point))))
    (while (= (line-number-at-pos (point)) line)
      (next-line))))

(defun yafolding-next-line-exists-p()
  "Test if next line exists"
  (< (line-end-position)
     (line-number-at-pos (point-max))))

(defun yafolding-next-line-exists-p()
  "Test if next line exists"
  (< (line-number-at-pos (point))
     (line-number-at-pos (point-max))))


(defun yafolding-get-overlay ()
  "Return the yafolding's overlay in current line"
  (save-excursion
    (let ((overlays (overlays-in
                     (line-beginning-position)
                     (line-end-position)))
          (overlay)
          (the-overlay)
          (point-delta 0))
      (while (car overlays)
        (setq overlay (pop overlays))
        ;; get the outer overlay
        (when (member "zeno-folding" (overlay-properties overlay))
          (let ((overlay-point-delta (- (overlay-end overlay)
                                        (overlay-start overlay))))
            (when (> overlay-point-delta point-delta)
              (setq point-delta overlay-point-delta)
              (setq the-overlay overlay)))))
      the-overlay)))


;; 分隔符 ------------------

(defun yafolding-get-level()
  (defun yafolding-get-level-iter()
    (if (<= (yafolding-get-column) (car levels))
	(progn
	  (pop levels)
	  (yafolding-get-level-iter))
      (progn
	(push (yafolding-get-column) levels)
	(length levels))))
  (if (= 0 (yafolding-get-column))
      (progn
	(setq levels '(0))
	1)
    (yafolding-get-level-iter)))


(defun yafolding ()
  "floding based on indeneation"
  (interactive)

  (defun yafolding-get-first-line-data()
    (save-excursion
      (while (and
	      (yafolding-line-string-match-p "^[ \t]*$")
	      (yafolding-next-line-exists-p))
	(yafolding-my-next-line))
      (if (yafolding-line-string-match-p "^[ {\t]*$")
	  (setq first-line-data "{"))
      (if (yafolding-line-string-match-p "^[ \\(\t]*$")
	  (setq first-line-data "("))))

  (defun yafolding-get-last-line-data()
    (save-excursion
      (while (and
	      (yafolding-line-string-match-p "^[ \t]*$")
	      (previous-line-exists-p))
	(previous-line))
      (if (yafolding-line-string-match-p "^[ });\t]*$")
	  (setq last-line-data "});"))
      (if (yafolding-line-string-match-p "^[ };\t]*$")
	  (setq last-line-data "};"))
      (if (yafolding-line-string-match-p "^[ }\t]*$")
	  (setq last-line-data "}"))
      (if (yafolding-line-string-match-p "^[ )\t]*$")
	  (setq last-line-data ")"))
      ))

  (defun yafolding-show ()
    (save-excursion
      (delete-overlay (yafolding-get-overlay))))

  (defun yafolding-hide ()
    (save-excursion
      (let* ((parent-level (yafolding-get-column))
	     (beg (line-end-position))
	     (end beg)
	     (first-line-data)
	     (last-line-data))
	(yafolding-my-next-line)
	(yafolding-get-first-line-data)
	(when (is-child)
	  (while (and (is-child)
		      (yafolding-next-line-exists-p))
	    (yafolding-my-next-line))
	  (unless (is-child)
	    (previous-line))
	  (setq end (line-end-position))
	  (yafolding-get-last-line-data)

	  ;; 若仅仅为空行，则不处理
	  (if (string-match-p "[^ \t\n\r]+" (buffer-substring beg end))
	      (let ((new-overlay (make-overlay beg end)))
		(overlay-put new-overlay 'invisible t)
		(overlay-put new-overlay 'intangible t)
		(overlay-put new-overlay 'isearch-open-invisible-temporary 'hs-isearch-show-temporary)
		(overlay-put new-overlay 'modification-hooks
			     (list (lambda (overlay &optional a b c d)
				     (delete-overlay overlay))))
		(overlay-put new-overlay 'category "zeno-folding")

		;; for emacs-lisp-mode
		(if (and
		     (equal major-mode 'emacs-lisp-mode)
		     (not last-line-data))
		    (setq last-line-data ")"))

		(if first-line-data
		    (overlay-put new-overlay 'before-string
				 (concat first-line-data "..."))
		  (overlay-put new-overlay 'before-string "..."))
		(if last-line-data
		    (overlay-put new-overlay 'after-string last-line-data))))))))




  (defun previous-line-exists-p()
    (save-excursion
      (search-backward "\n" nil t nil)))

  (defun is-child()
    (or (> (yafolding-get-column) parent-level)
	(and (= (yafolding-get-column) parent-level)
	     (yafolding-line-string-match-p "^[ {});\t]*$"))
	(yafolding-line-string-match-p "^[ \t]*$")))

  (if (yafolding-get-overlay)
      (yafolding-show)
    (if (yafolding-line-string-match-p "[^ \t]+")
	(yafolding-hide))))


(defun yafolding-hide-all(level)
  (interactive "nLevel:")
  (defun yafolding-line-string-match-p(regexp)
    (string-match-p regexp
		    (buffer-substring-no-properties
		     (line-beginning-position)
		     (line-end-position))))
  (yafolding-show-all)
  ;; level => column
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (let ((levels '(0)))
	(while (yafolding-next-line-exists-p)
	  (yafolding-my-next-line)
	  (unless (yafolding-line-string-match-p "^[ \t]$")
	    (forward-char) ; 防止停留在overlay的最后导致重复toggle
	    (when (= (yafolding-get-level) level)
	      (yafolding))))))))

(defun yafolding-show-all()
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max)))
	(overlay))
    (while (car overlays)
      (setq overlay (pop overlays))
      (if (member "zeno-folding" (overlay-properties overlay))
	  (delete-overlay overlay)))))
(defun yafolding-toggle-all(&optional level)
  (interactive)
  (unless level
    (setq level 1))
  (message "lev:%d" level)
  (let ((overlays (overlays-in (point-min) (point-max)))
	(overlay)
	(previous-hide-p nil))
    (while (car overlays)
      (setq overlay (pop overlays))
      (when (member "zeno-folding" (overlay-properties overlay))
	(delete-overlay overlay)
	(setq previous-hide-p t)))
    (unless previous-hide-p
      (yafolding-hide-all level))))

(defun yafolding-temp-toggle(hide-p)
  (let ((overlays (overlays-in (point-min) (point-max)))
	(overlay))
    (while (car overlays)
      (setq overlay (pop overlays))
      (if (member "zeno-folding" (overlay-properties overlay))
	  (overlay-put overlay 'invisible hide-p)))))

(defun yafolding-get-current-line-level()
  (interactive)
  (let ((line (line-number-at-pos))
	(levels '(0))
	(result 1))
    (goto-char (point-min))
    (while (and (yafolding-next-line-exists-p) (< (line-number-at-pos) line))
      (yafolding-my-next-line)
      (unless (yafolding-line-string-match-p "^[ \t]$")
	(forward-char) ; 防止停留在overlay的最后导致重复toggle
	(yafolding-get-level)
	(if (= (line-number-at-pos) line)
	    (setq result (yafolding-get-level)))))
    result))

(defun yafolding-toggle-all-by-current-level()
  (interactive)
  (message "%s" (yafolding-get-current-line-level))
  (yafolding-toggle-all (yafolding-get-current-line-level)))

(add-hook 'isearch-mode-hook (lambda() (yafolding-temp-toggle nil)))
(add-hook 'isearch-mode-end-hook (lambda() (yafolding-temp-toggle t)))

(provide 'yafolding)
;;; yafolding.el ends here
