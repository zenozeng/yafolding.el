;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have an empty buffer$"
       (lambda () (erase-buffer)))

(When "^I call yafolding-hide-all$"
      (lambda () (yafolding-hide-all)))

(Then "^I should have an empty buffer$"
      (lambda () (should (= (point-min) (point-max) 1))))
