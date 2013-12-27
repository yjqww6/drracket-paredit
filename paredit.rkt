#lang s-exp framework/keybinding-lang
(require drracket/tool-lib)
;;;; my shortcuts

;; Movement
(define (get-my-forward-sexp ed sp)
  (cond
    [(send ed get-forward-sexp sp)
     => (λ (pos) pos)]
    [(send ed find-up-sexp sp)
     => (λ (pos)
          (send ed get-forward-sexp pos))]
    [else #f]))

(define (my-forward-sexp ed evt)
  (define sp (send ed get-start-position))
  (define dest (get-my-forward-sexp ed sp))
  (when dest
    (send ed set-position dest)))

(define (get-my-backward-sexp ed sp)
  (cond
    [(send ed get-backward-sexp sp)
     => (λ (pos) pos)]
    [else (send ed find-up-sexp sp)]))

(define (my-backward-sexp ed evt)
  (define sp (send ed get-start-position))
  (define dest (get-my-backward-sexp ed sp))
  (when dest
    (send ed set-position dest)))

(define (my-forward-term ed evt)
  (define sp (send ed get-start-position))
  (define dests 
    (filter (λ (x) x)
            (list (send ed find-down-sexp sp)
                  (get-my-forward-sexp ed sp))))
  (unless (null? dests)
    (send ed set-position (apply min dests))))

(define (find-down-sexp-backward ed pos)
  (define bw (send ed get-backward-sexp pos))
  (if (not bw)
      #f
      (let ([down (send ed find-down-sexp bw)])
        (if (or (not down) (> down pos))
            #f
            (last-sexp ed down)))))

(define (my-backward-term ed evt)
  (define sp (send ed get-start-position))
  (define dests 
    (filter (λ (x) x)
            (list (find-down-sexp-backward ed sp)
                  (get-my-backward-sexp ed sp))))
  (unless (null? dests)
    (send ed set-position (apply max dests))))

(keybinding "c:m:b" my-backward-sexp)
(keybinding "c:m:f" my-forward-sexp)
(keybinding "m:left" my-backward-term)
(keybinding "m:right" my-forward-term)
(keybinding "c:m:d"
            (λ (ed evt)
              (send ed down-sexp
                    (send ed get-start-position))))



(define (key-binding key proc)
  (keybinding key 
              (λ (ed evt) 
                (send ed begin-edit-sequence)
                (proc ed evt)
                (send ed end-edit-sequence))))

;;; Depth-Changing
(define (splice-sexp ed evt [pos #f])
  (when (not pos)
    (set! pos (send ed get-start-position)))
  (let ([begin-outer (send ed find-up-sexp pos)])
    (if begin-outer
        (let ([end-outer (send ed get-forward-sexp begin-outer)])
          (cond
            [end-outer
             (send ed delete (- end-outer 1) end-outer)
             (send ed delete begin-outer (+ begin-outer 1))]
            [else (bell)]))
        (bell))))

(define (start-of-sexp ed pos)
  (define fw (send ed get-forward-sexp pos))
  (cond [(if fw
             (send ed get-backward-sexp fw)
             (send ed get-backward-sexp pos))
         => (λ (v) v)]
        [else pos]))

(define (sexp-start ed)
  (start-of-sexp ed (send ed get-start-position)))

(define (wrap-round ed evt)
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

(define (splice-sexp-killing-backward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-backward ed sp)
    (splice-sexp ed evt)))

(define (splice-sexp-killing-forward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-forward ed sp)
    (splice-sexp ed evt)))

(define (raise-sexp ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (define fw (send ed get-forward-sexp sp))
    (when fw
      (kill-sexps-forward ed fw))
    (kill-sexps-backward ed sp)
    (splice-sexp ed evt)))

(define (convolute-sexp ed evt)
  (define sp (sexp-start ed))
  (cond 
    [(send ed find-up-sexp sp) 
     => (λ (r1)
          (cond 
            [(send ed find-up-sexp r1)
             => (λ (r2)
                  (define text (send ed get-text r1 sp))
                  (let ([end (send ed get-forward-sexp r2)])
                    (send ed insert ")" end)
                    (kill-sexps-backward ed sp)
                    (splice-sexp ed evt (+ r1 1))
                    (send ed insert text r2)))]
            [else (bell)]))]
    [else (bell)]))

(key-binding "m:s" splice-sexp)
(key-binding "m:(" wrap-round)
(key-binding "m:up" splice-sexp-killing-backward)
(key-binding "m:down" splice-sexp-killing-forward)
(key-binding "m:r" raise-sexp)
(key-binding "m:?" convolute-sexp)

;;;Barfage & Slurpage
;;; only process reversible cases

(define (find-slurp-forward ed pos)
  (call/ec
   (λ (break)
     (define up (send ed find-up-sexp pos))
     (when (not up)
       (break #f))
     (define end (send ed get-forward-sexp up))
     (when (not end)
       (break #f))
     (define fw (send ed get-forward-sexp end))
     (if fw
         end
         #f))))

(define (slurp-forward ed evt)
  (define sp (send ed get-start-position))
  (define end (find-slurp-forward ed sp))
  (when end
    (define fw (send ed get-forward-sexp end))
    (send ed insert ")" fw)
    (send ed delete end)))

(define (find-slurp-backward ed pos)
  (call/ec
   (λ (break)
     (define up (send ed find-up-sexp pos))
     (when (not up)
       (break #f))
     (define bw (send ed get-backward-sexp up))
     (if bw
         up
         #f))))

(define (slurp-backward ed evt)
  (define sp (send ed get-start-position))
  (define start (find-slurp-backward ed sp))
  (when start
    (define bw (send ed get-backward-sexp start))
    (send ed delete (+ start 1))
    (send ed insert "(" bw)))

(key-binding "c:right" slurp-forward)
(key-binding "c:m:left" slurp-backward)

(define (barf-forward ed evt)
  (define sp (send ed get-start-position))
  (define up (send ed find-up-sexp sp))
  (when up
    (define fw (send ed get-forward-sexp up))
    (when fw
      (define last (last-sexp ed sp))
      (define bw (send ed get-backward-sexp last))
      (when bw
        (let ([bw1 (send ed get-backward-sexp bw)])
          (when bw1
            (cond [(send ed get-forward-sexp bw1)
                   => (λ (x) (set! bw x))]
                  [else (void)])))
        (send ed delete fw)
        (send ed insert ")" bw)
        (send ed set-position sp)))))

(define (barf-backward ed evt)
  (define sp (send ed get-start-position))
  (define up (send ed find-up-sexp sp))
  (when up
    (define down (send ed find-down-sexp up))
    (when down
      (define fw (send ed get-forward-sexp down))
      (when fw
        (define fw1 (send ed get-forward-sexp fw))
        (when fw1
          (cond [(send ed get-backward-sexp fw1)
                 => (λ (x) (set! fw x))]
                [else (void)]))
        (send ed insert "(" fw)
        (send ed delete (+ up 1))))))

(key-binding "c:left" barf-forward)
(key-binding "c:m:right" barf-backward)