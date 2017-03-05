;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have an empty buffer$"
       (lambda () (erase-buffer)))

(When "^I call yafolding-hide-all$"
      (lambda () (yafolding-hide-all)))

(Then "^I should have an empty buffer$"
      (lambda () (should (= (point-min) (point-max) 1))))

(Given "^I have a buffer with \"\\([^\"]+\\)\"$"
       (lambda (filename)
         (erase-buffer)
         (insert-file-contents filename)))

(And "^I \\(?:am on\\|go to\\) line \\([0-9]+\\)$"
     (lambda (line) (goto-line (string-to-number line))))

(And "^I call yafolding-hide-element$"
     (lambda () (yafolding-hide-element)))

(Then "^I should see \\(?:only \\)?\\([0-9]+\\) lines?$"
      (lambda (num-lines)
        (save-excursion
          (beginning-of-buffer)
          (next-line (string-to-number num-lines))
          (should (= (line-number-at-pos (point))
                     (line-number-at-pos (point-max)))))))

;; End of steps file
