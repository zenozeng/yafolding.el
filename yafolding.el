;;; yafolding.el --- Yet another folding extension for Emacs

;; Copyright (C) 2013  Zeno Zeng

;; Author: Zeno Zeng <zenoes@qq.com>
;; keywords: 

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

;;;###autoload
(defun yafolding ()
  "floding based on indeneation"
  (interactive)
  (defun get-overlay ()
    (save-excursion
      (let ((overlays (overlays-in
		       (progn (move-beginning-of-line nil) (point))
		       (progn (move-end-of-line nil) (point))))
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

  (defun get-first-line-data()
    (save-excursion
      (while (and
	      (line-string-match-p "^[ \t]*$")
	      (next-line-exists-p))
	(my-next-line))
      (if (line-string-match-p "^[ {\t]*$")
	  (setq first-line-data "{"))
      (if (line-string-match-p "^[ \\(\t]*$")
	  (setq first-line-data "("))
      ))

  (defun get-last-line-data()
    (save-excursion
      (while (and
	      (line-string-match-p "^[ \t]*$")
	      (previous-line-exists-p))
	(previous-line))
      (if (line-string-match-p "^[ }\t]*$")
	  (setq last-line-data "}"))
      (if (line-string-match-p "^[ \\)\t]*$")
	  (setq last-line-data ")"))
      ))

  (defun show ()
    (save-excursion
      (delete-overlay (get-overlay))))

  (defun hide ()
    (save-excursion
      (let* ((parent-level (get-column))
	     (beg (line-end-position))
	     (end beg)
	     (first-line-data)
	     (last-line-data))
	(my-next-line)
	(get-first-line-data)
	(when (is-child)
	  (while (and (is-child)
		      (next-line-exists-p))
	    (my-next-line))
	  (unless (is-child)
	    (previous-line))
	  (setq end (line-end-position))
	  (get-last-line-data)

	  ;; 若仅仅为空行，则不处理
	  (if (string-match-p "[^ \t\n\r]+" (buffer-substring beg end))
	      (let ((new-overlay (make-overlay beg end)))
		(overlay-put new-overlay 'invisible t)
		(overlay-put new-overlay 'intangible t)
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

  (defun get-column()
    (back-to-indentation)
    (current-column))

  (defun line-string-match-p(regexp)
    (string-match-p regexp
		    (buffer-substring-no-properties
		     (line-beginning-position)
		     (line-end-position))))

  (defun next-line-exists-p()
    (< (line-number-at-pos (point))
       (line-number-at-pos (point-max))))

  (defun previous-line-exists-p()
    (save-excursion
      (search-backward "\n" nil t nil)))

  (defun is-child()
    (or (> (get-column) parent-level)
	(and (= (get-column) parent-level)
	     (line-string-match-p "^[ {}\t]*$"))
	(line-string-match-p "^[ \t]*$")))

  (defun my-next-line()
    "next logical line and skip overlay at the same time"
    (let ((line (line-number-at-pos (point))))
      (while (= (line-number-at-pos (point)) line)
	(next-line))))

  (if (get-overlay)
      (show)
    (if (line-string-match-p "[^ \t]+")
	(hide))))


;;;###autoload
(defun yafolding-hide-all(level)
  (interactive "nLevel:")
  (defun line-string-match-p(regexp)
    (string-match-p regexp
		    (buffer-substring-no-properties
		     (line-beginning-position)
		     (line-end-position))))
  (defun get-column()
    (back-to-indentation)
    (current-column))

  (defun my-next-line()
    "next logical line and skip overlay at the same time"
    (let ((line (line-number-at-pos (point))))
      (while (= (line-number-at-pos (point)) line)
	(next-line))))

  (defun next-line-exists-p()
    (< (line-number-at-pos (point))
       (line-number-at-pos (point-max))))

  (defun get-level()
    (defun iter()
      (if (<= (get-column) (car levels))
	  (progn
	    (pop levels)
	    (iter))
	(progn
	  (push (get-column) levels)
	  (length levels))))
    (if (= 0 (get-column))
	(progn
	  (setq levels '(0))
	  1)
      (iter)))
  
  (yafolding-show-all)
  ;; level => column
  (ignore-errors
    (save-excursion
      (goto-char (point-min))
      (let ((levels '(0)))
	(while (next-line-exists-p)
	  (my-next-line)
	  (unless (line-string-match-p "^[ \t]$")
	    (forward-char) ; 防止停留在overlay的最后导致重复toggle
	    (when (= (get-level) level)
	      (yafolding))))))))

;;;###autoload
(defun yafolding-show-all()
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max)))
	(overlay))
    (while (car overlays)
      (setq overlay (pop overlays))
      (if (member "zeno-folding" (overlay-properties overlay))
	  (delete-overlay overlay)))))



(provide 'yafolding)
;;; yafolding.el ends here
