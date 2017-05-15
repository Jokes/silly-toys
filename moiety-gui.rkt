#lang racket/gui

(require "moiety.rkt" "bytes.rkt" racket/block)

(define (col-from rgb)
  (if (equal? rgb "")
      (make-color 0 0 0)
      (apply make-color (hex->bytes (substring rgb 1)))))

(define moiety-hash
  (apply hash
         (apply append
                (map (λ (a b) (list a b))
                     (map Moiety->string moiety-rainbow)
                     moiety-rainbow))))
(define current-moiety (moiety "Nobody" "Black" "#000000"))
(define (swap-moiety s)
  (set! current-moiety 
        (if (hash-has-key? moiety-hash s)
            (hash-ref moiety-hash s)
            (moiety "Nobody" "Black" "#000000"))))


(define τ (* 2 3.14159265358979))
(define rot (* τ 1/4))

(define (draw-circle dc x y r)
  (send dc draw-ellipse (- x r) (- y r) (* r 2) (* r 2)))
(define (draw-segment dc x y r n [nib 1])
  (let ([nibble (* (/ τ 67) (* nib 0.05))])
    (send dc draw-arc (- x r) (- y r) (* r 2) (* r 2)
          (+ rot (* (sub1 n) (/ τ 67)) nibble) (+ rot (- (* n (/ τ 67)) nibble)))))

(define (draw-things dc)
  (let*-values
      ([(width height) (send dc get-size)] 
       [(midwidth) (/ width 2)] [(midheight) (/ height 2)]
       [(smaller-bound) (if (< width height) width height)] [(inrad-base) (/ smaller-bound 4)]
       [(ring-breadth) (/ inrad-base 3)] [(ring-inner) (/ ring-breadth 3)]
       [(ring-boundary) (* ring-inner 2/3)]
       [(pcol) (col-from (Moiety-hex1 current-moiety))])
    (send dc set-background (make-color 240 240 240))
    (send dc clear)
    (send dc set-smoothing 'smoothed)
    (send dc set-pen pcol ring-boundary 'solid)
    
    (if (Moiety-secondary current-moiety)
        (let ([scol (col-from (Moiety-hex2 current-moiety))])
          (send dc set-brush scol 'solid)
          (draw-circle dc midwidth midheight (- (* ring-breadth 6) ring-boundary))
          (send dc set-pen scol ring-boundary 'solid)
          (send dc set-brush pcol 'solid)
          (draw-circle dc midwidth midheight (- (* ring-breadth 4) ring-boundary))
          )
        (block
         (send dc set-brush pcol 'solid)
         (draw-circle dc midwidth midheight (- (* ring-breadth 6) ring-boundary))
         ))
    ))


(define base-frame (new frame% 
                        [label "Moiety Flag Generator"]
                        [width 800]
                        [height 600]))
(send base-frame create-status-line)
(define (set-status str)
  (send base-frame set-status-text str))

(define base-panel (new vertical-panel% [parent base-frame]))

(define extant-moiety-combo 
  (new choice%
       [label "Author"]
       [parent base-panel]
       [choices (cons "(not listed)" (map Moiety->string moiety-rainbow))]
       [callback (λ (c e)
                   (let ([s (send c get-string-selection)])
                     (swap-moiety s)
                     (set-status s))
                   (send canvas refresh))]))

(define canvas (new canvas% [parent base-panel] [min-width 10] [min-height 10]
                    [paint-callback
                     (λ (canvas dc)
                       (draw-things dc))]))

(send base-frame show #t)
