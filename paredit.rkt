#lang s-exp framework/keybinding-lang
(require drracket/tool-lib srfi/2)
(require (for-syntax racket/list))

;; The raw version of define-shortcut that does not perform any
;; key processing or body wrapping.
(define-syntax-rule (define-shortcut-internal (key ...) name proc)
  (begin
    (define (name ed evt . rest)
      (when (is-a? ed racket:text<%>)
        (send ed begin-edit-sequence)
        (apply proc ed evt rest)
        (send ed end-edit-sequence)))
    (keybinding key name) ...))

(define-syntax (define-shortcut stx)
  ;; Add esc; equivalent key bindings for all the meta bindings.
  (define (add-esc-and-option-key-bindings s-keys)
    (define keys (syntax->datum s-keys))
    (define esc-variants
      (for/list ([k (in-list keys)]
                 #:when (regexp-match? #rx"m:" k))
        (string-append "esc;" (regexp-replace* #rx"m:" k ""))))
    (define option-variants
      (for/list ([k (in-list keys)]
                 #:when (regexp-match? #rx"m:" k))
        (string-append "?:a:" (regexp-replace* #rx"m:" k ""))))
    ;; Use remove-duplicates to combine all key bindings, so that duplicates
    ;; are removed. This means that if we add some esc; key bindings manually,
    ;; for example by accident, it will not be duplicated, affecting display
    ;; of key bindings in DrRacket.
    (remove-duplicates (append esc-variants option-variants keys)))
  (syntax-case stx ()
    [(_ key (name . args) body* ...)
     #'(define-shortcut key name
         (λ args body* ...))]
    [(_ (key ...) name proc)
     #`(define-shortcut-internal
         (#,@(add-esc-and-option-key-bindings #'(key ...)))
         name proc)]
    [(_ key name proc)
     #'(define-shortcut (key) name proc)]))

;;; Movement
(define (get-paredit-forward-sexp ed sp)
  (cond
    [(send ed get-forward-sexp sp)
     => (λ (pos) pos)]
    [(send ed find-up-sexp sp)
     => (λ (pos)
          (send ed get-forward-sexp pos))]
    [else #f]))

(define-shortcut ("c:m:f") (paredit-forward-sexp ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([dest (get-paredit-forward-sexp ed sp)])
    (send ed set-position dest)))

(define (get-paredit-backward-sexp ed sp)
  (cond
    [(send ed get-backward-sexp sp)
     => (λ (pos) pos)]
    [else (send ed find-up-sexp sp)]))

(define-shortcut ("c:m:b") (paredit-backward-sexp ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([dest (get-paredit-backward-sexp ed sp)])
    (send ed set-position dest)))

(define-shortcut ("c:m:d") (paredit-down-sexp ed evt)
  (send ed down-sexp
        (send ed get-start-position)))

(define (get-forward-atom ed pos)
  (define sp (send ed get-start-position))
  (define dests 
    (filter (λ (x) x)
            (list (send ed find-down-sexp sp)
                  (get-paredit-forward-sexp ed sp))))
  (and (not (null? dests)) (apply min dests)))

(define-shortcut ("m:right") (forward-atom ed evt)
  (and-let* ([dest (get-forward-atom ed (send ed get-start-position))])
    (send ed set-position dest)))

(define (find-down-sexp-backward ed pos)
  (and-let* ([bw (send ed get-backward-sexp pos)]
             [down (send ed find-down-sexp bw)])
    (if (or (not down) (> down pos))
        #f
        (last-sexp ed down))))

(define (get-backward-atom ed pos)
  (define sp (send ed get-start-position))
  (define dests 
    (filter (λ (x) x)
            (list (find-down-sexp-backward ed sp)
                  (get-paredit-backward-sexp ed sp))))
  (and (not (null? dests)) (apply max dests)))

(define-shortcut ("m:left") (backward-atom ed evt)
  (and-let* ([dest (get-backward-atom ed (send ed get-start-position))])
    (send ed set-position dest)))

;;; Depth-Changing
(define-shortcut ("m:s") (paredit-splice-sexp ed evt [pos #f] [reindent #t])
  (when (not pos)
    (set! pos (send ed get-start-position)))
  (and-let* ([begin-outer (send ed find-up-sexp pos)]
             [end-outer (send ed get-forward-sexp begin-outer)])
    (send ed delete (- end-outer 1) end-outer)
    (send ed delete begin-outer (+ begin-outer 1))
    (when reindent
      (send ed tabify-selection begin-outer end-outer))))

(define (start-of-sexp ed pos)
  (define fw (send ed get-forward-sexp pos))
  (cond [(if fw
             (send ed get-backward-sexp fw)
             (send ed get-backward-sexp pos))
         => (λ (v) v)]
        [else pos]))

(define (sexp-start ed)
  (start-of-sexp ed (send ed get-start-position)))

(define-shortcut ("m:(") (paredit-wrap-round ed evt)
  (send ed insert "(")
  (let ([pos (send ed get-start-position)])
    (send ed forward-sexp pos)
    (send ed insert ")")
    (send ed set-position pos)))

(define (first-sexp ed sp)
  (let loop ([pos sp] [prev sp])
    (if pos
        (loop (send ed get-backward-sexp pos) pos)
        prev)))

(define (last-sexp ed sp)
  (let loop ([pos sp] [prev sp])
    (if pos
        (loop (send ed get-forward-sexp pos) pos)
        prev)))

(define (kill-sexps-backward ed pos)
  (send ed delete (first-sexp ed pos) pos))

(define (kill-sexps-forward ed pos)
  (send ed delete pos (last-sexp ed pos)))

(define (not-toplevel? ed pos)
  (send ed find-up-sexp pos))

(define-shortcut ("m:up") (paredit-splice-sexp-killing-backward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-backward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("m:down") (paredit-splice-sexp-killing-forward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-forward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("m:r") (paredit-raise-sexp ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (and-let* ([fw (send ed get-forward-sexp sp)])
      (kill-sexps-forward ed fw))
    (kill-sexps-backward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("m:?") (paredit-convolute-sexp ed evt)
  (define sp (sexp-start ed))
  (and-let* ([r1 (send ed find-up-sexp sp)]
             [fw (send ed get-forward-sexp r1)]
             [paren (send ed get-text (- fw 1) fw)]
             [r2 (send ed find-up-sexp r1)]
             [text (send ed get-text r1 sp)]
             [end (send ed get-forward-sexp r2)])
    (send ed insert paren end)
    (kill-sexps-backward ed sp)
    (paredit-splice-sexp ed evt (+ r1 1) #f)
    (send ed insert text r2)
    (send ed tabify-selection r2 end)))


;;;Barfage & Slurpage

(define (find-up-sexp-slurp-forward ed sp)
  (let loop ([sp (send ed find-up-sexp sp)])
    (cond [(not sp) #f]
          [(and-let* ([fw1 (send ed get-forward-sexp sp)])
             (send ed get-forward-sexp fw1)) sp]
          [else (loop (send ed find-up-sexp sp))])))

(define-shortcut ("c:right" "c:s:0" "c:]") (paredit-slurp-forward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([up (find-up-sexp-slurp-forward ed sp)]
             [end (send ed get-forward-sexp up)]
             [fw (send ed get-forward-sexp end)]
             [paren (send ed get-text (- end 1) end)])
    (send ed insert paren fw)
    (send ed delete end)
    (send ed tabify-selection up fw)))

(define (find-up-sexp-slurp-backward ed sp)
  (let loop ([sp (send ed find-up-sexp sp)])
    (cond [(not sp) #f]
          [(send ed get-backward-sexp sp) sp]
          [else (loop (send ed find-up-sexp sp))])))

(define-shortcut ("c:m:left" "c:s:9" "c:[") (paredit-slurp-backward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([start (find-up-sexp-slurp-backward ed sp)]
             [bw (send ed get-backward-sexp start)]
             [paren (send ed get-text start (+ start 1))])
    (send ed delete (+ start 1))
    (send ed insert paren bw)
    (send ed tabify-selection bw start)))

(define-shortcut ("c:left" "c:}") (paredit-barf-forward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([up (send ed find-up-sexp sp)]
             [fw (send ed get-forward-sexp up)]
             [paren (send ed get-text (- fw 1) fw)]
             [last (last-sexp ed sp)]
             [bw (send ed get-backward-sexp last)])
    (and-let* ([bw1 (send ed get-backward-sexp bw)]
               [x (send ed get-forward-sexp bw1)])
      (set! bw x))
    (send ed delete fw)
    (send ed insert paren bw)
    (send ed set-position sp)
    (send ed tabify-selection up fw)))

(define-shortcut ("c:m:right" "c:{") (paredit-barf-backward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([up (send ed find-up-sexp sp)]
             [paren (send ed get-text up (+ up 1))]
             [down (send ed find-down-sexp up)]
             [fw (send ed get-forward-sexp down)])
    (and-let* ([fw1 (send ed get-forward-sexp fw)]
               [x (send ed get-backward-sexp fw1)])
      (set! fw x))
    (send ed insert paren fw)
    (send ed delete (+ up 1))
    (send ed tabify-selection up fw)))