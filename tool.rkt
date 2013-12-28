#lang racket
(require drracket/tool framework racket/runtime-path)
(provide tool@)

(define-runtime-path file "paredit.rkt")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define (phase1) (void))
    (define (phase2) (void))
    (keymap:add-user-keybindings-file file)))