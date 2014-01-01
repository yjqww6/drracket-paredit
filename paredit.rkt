#lang s-exp framework/keybinding-lang
(require drracket/tool-lib)

(define-syntax let*-when
  (syntax-rules ()
    [(_ () body* ...) (begin body* ...)]
    [(_ ([id form] bind* ...) body* ...)
     (let ([id form])
       (when id
         (let*-when (bind* ...)
           body* ...)))]))

(define-syntax-rule (let*-when/false (bind* ...) body* ...)
  (begin
    (define ret
      (let*-when (bind* ...) body* ...))
    (if (void? ret) #f ret)))

(define-syntax define-shortcut
  (syntax-rules ()
    [(_ key (name . args) body* ...)
     (define-shortcut key name
       (λ args body* ...))]
    [(_ (key ...) name proc)
     (begin
       (define (name ed evt . rest)
         (when (is-a? ed racket:text<%>)
           (send ed begin-edit-sequence)
           (apply proc ed evt rest)
           (send ed end-edit-sequence)))
       (for ([k (in-list (list key ...))])
         (keybinding k name)))]
    [(_ key name proc)
     (define-shortcut (key) name proc)]))

;;; Movement
(define (get-paredit-forward-sexp ed sp)
  (cond
    [(send ed get-forward-sexp sp)
     => (λ (pos) pos)]
    [(send ed find-up-sexp sp)
     => (λ (pos)
          (send ed get-forward-sexp pos))]
    [else #f]))

(define-shortcut ("c:m:f" "esc;c:f") (paredit-forward-sexp ed evt)
  (define sp (send ed get-start-position))
  (let*-when ([dest (get-paredit-forward-sexp ed sp)])
    (send ed set-position dest)))

(define (get-paredit-backward-sexp ed sp)
  (cond
    [(send ed get-backward-sexp sp)
     => (λ (pos) pos)]
    [else (send ed find-up-sexp sp)]))

(define-shortcut ("c:m:b" "esc;c:b") (paredit-backward-sexp ed evt)
  (define sp (send ed get-start-position))
  (let*-when ([dest (get-paredit-backward-sexp ed sp)])
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
  (and (not (null? dests))
       (apply min dests)))

(define-shortcut ("m:right" "esc;right") (forward-atom ed evt)
  (let*-when ([dest (get-forward-atom ed (send ed get-start-position))])
    (send ed set-position dest)))

(define (find-down-sexp-backward ed pos)
  (let*-when/false ([bw (send ed get-backward-sexp pos)]
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

(define-shortcut ("m:left" "esc;left") (backward-atom ed evt)
  (let*-when ([dest (get-backward-atom ed (send ed get-start-position))])
    (send ed set-position dest)))

;;; Depth-Changing
(define-shortcut ("m:s" "esc;s") (paredit-splice-sexp ed evt [pos #f] [reindent #t])
  (when (not pos)
    (set! pos (send ed get-start-position)))
  (let*-when ([begin-outer (send ed find-up-sexp pos)]
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

(define-shortcut ("m:(" "esc;(") (paredit-wrap-round ed evt)
  (send ed insert "(")
  (send ed forward-sexp (send ed get-start-position))
  (send ed insert ")"))

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

(define-shortcut ("m:up" "esc;up") (paredit-splice-sexp-killing-backward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-backward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("m:down" "esc;down") (paredit-splice-sexp-killing-forward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-forward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("m:r" "esc;r") (paredit-raise-sexp ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (let*-when ([fw (send ed get-forward-sexp sp)])
      (kill-sexps-forward ed fw))
    (kill-sexps-backward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("m:?" "esc;?") (paredit-convolute-sexp ed evt)
  (define sp (sexp-start ed))
  (let*-when ([r1 (send ed find-up-sexp sp)]
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
;;; only process reversible cases

(define-shortcut ("c:right" "c:s:0" "c:]") (paredit-slurp-forward ed evt)
  (define sp (send ed get-start-position))
  (let*-when ([up (send ed find-up-sexp sp)]
              [end (send ed get-forward-sexp up)]
              [fw (send ed get-forward-sexp end)]
              [paren (send ed get-text (- end 1) end)])
    (send ed insert paren fw)
    (send ed delete end)
    (send ed tabify-selection fw end)))

(define-shortcut ("c:m:left" "esc;c:left" "c:s:9" "c:[") (paredit-slurp-backward ed evt)
  (define sp (send ed get-start-position))
  (let*-when ([start (send ed find-up-sexp sp)]
              [bw (send ed get-backward-sexp start)]
              [paren (send ed get-text start (+ start 1))])
    (send ed delete (+ start 1))
    (send ed insert paren bw)
    (send ed tabify-selection bw start)))

(define-shortcut ("c:left" "c:}") (paredit-barf-forward ed evt)
  (define sp (send ed get-start-position))
  (let*-when ([up (send ed find-up-sexp sp)]
              [fw (send ed get-forward-sexp up)]
              [paren (send ed get-text (- fw 1) fw)]
              [last (last-sexp ed sp)]
              [bw (send ed get-backward-sexp last)])
    (let*-when ([bw1 (send ed get-backward-sexp bw)]
                [x (send ed get-forward-sexp bw1)])
      (set! bw x))
    (send ed delete fw)
    (send ed insert paren bw)
    (send ed set-position sp)
    (send ed tabify-selection bw fw)))

(define-shortcut ("c:m:right" "esc;c:right" "c:{") (paredit-barf-backward ed evt)
  (define sp (send ed get-start-position))
  (let*-when ([up (send ed find-up-sexp sp)]
              [paren (send ed get-text up (+ up 1))]
              [down (send ed find-down-sexp up)]
              [fw (send ed get-forward-sexp down)])
    (let*-when ([fw1 (send ed get-forward-sexp fw)]
                [x (send ed get-backward-sexp fw1)])
      (set! fw x))
    (send ed insert paren fw)
    (send ed delete (+ up 1))
    (send ed tabify-selection up fw)))