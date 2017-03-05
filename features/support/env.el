(require 'f)

(defvar yafolding-support-path
  (f-dirname load-file-name))

(defvar yafolding-features-path
  (f-parent yafolding-support-path))

(defvar yafolding-root-path
  (f-parent yafolding-features-path))

(add-to-list 'load-path yafolding-root-path)

(require 'yafolding)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
