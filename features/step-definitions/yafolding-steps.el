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

(And "^I am on line 2$"
     (lambda () (goto-line 2)))

(And "^I go to line 1$"
     (lambda () (goto-line 1)))

(And "^I call yafolding-hide-element$"
     (lambda () (yafolding-hide-element)))

(Then "^I should see only one line$"
      (lambda ()
        (save-excursion
          (next-line)
          (should (= (line-number-at-pos (point))
                     (line-number-at-pos (point-max)))))))

;; End of steps file
