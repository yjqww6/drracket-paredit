#lang setup/infotab

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names '("drracket-paredit"))
(define drracket-tool-icons '(#f))

(define collection "drracket-paredit")

(define name "drracket-paredit")
(define blurb '("some paredit shortcuts for DrRacket"))
(define primary-file "paredit.rkt")
(define deps '("base" "drracket" "drracket-plugin-lib" "gui-lib" "srfi-lib"))
