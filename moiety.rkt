#lang racket

(require "bytes.rkt")

(define (print-moiety name primary hex1 [secondary #f] [hex2 ""] [spoiler #f])
  (displayln 
   (if secondary
       (string-append
        "[color=" hex1 "]██[/color][color=" hex2 "]█[/color] " 
        (s-open spoiler) "[color=" hex1 "]" name ": [b]" primary "[/b], " hex1 "[/color] "
        "[size=85][color=" hex2 "]([b]" secondary "[/b], " hex2 ")[/color][/size]" (s-close spoiler))
       (string-append
        "[color=" hex1 "]███[/color] " 
        (s-open spoiler) "[color=" hex1 "]"name ": [b]" primary "[/b], " hex1 "[/color]" (s-close spoiler)))))

(define (s-open spoiler) (if spoiler "[spoiler]" ""))
(define (s-close spoiler) (if spoiler "[/spoiler]" ""))

(struct Moiety (name primary hex1 secondary hex2 spoiler) #:transparent)
(define (moiety name primary hex1 [secondary #f] [hex2 ""] [spoiler #f])
  (Moiety name primary (string-upcase hex1) secondary (string-upcase (if secondary hex2 hex1)) spoiler))

(define moiety-list
  (list
   (moiety "Alicorn" "Green" "#00BF80" "Cerulean" "#007BA7")
   (moiety "Kappa" "Purple" "#8040BF" "Indigo" "#602080")
   (moiety "Aestrix" "Blue" "#0080FF" "Navy" "#1a2196")
   (moiety "Marri" "Red" "#BF4040")
   (moiety "Benedict" "Grey" "#696969")
   (moiety "Anthusiasm" "Orange" "#FF8000")
   (moiety "PlainDealingVillain" "Tan" "#BF8040" "Orange" "#FF8000")
   (moiety "AndaisQ" "Pink" "#FF4080")
   (moiety "Anya" "Lavender" "#C080FF" "Silver" "#A0A0A0")
   (moiety "Lambda" "Teal" "#008080" "Brass" "#B5A642")
   (moiety "Kel" "Cyan" "#00CFCF" "Azure" "#007FFF")
   (moiety "Adelene" "Brown" "#62371F" "Stormcloud" "#AAB7BF")
   (moiety "CuriousDiscoverer" "Silver" "#A0A0A0")
   (moiety "Nemo" "Gold" "#FFBF00")
   (moiety "Link" "Violet" "#7F00FF")
   (moiety "Kuuskytkolme" "Lime" "#99C534")
   (moiety "ErinFlight" "Copper" "#D5732A")
   (moiety "MaggieoftheOwls" "White" "#FFFFFF" #f "" #t)
   (moiety "Rockeye" "Black" "#000000")
   (moiety "Pedro" "Salmon" "#ff5c5c" "Peach" "#FFB25C")
   (moiety "Eva" "Midnight" "#000080")
   (moiety "Teceler" "Forest" "#228B22")
   (moiety "Throne3d" "Carmine" "#960018" "Palatinate" "#682860")
   (moiety "Timepoof" "Rose" "#FF91BF" "Chocolate" "#692823")
   (moiety "RoboticLIN" "Venom" "#728C00")
   (moiety "Diaeresis" "Moss" "#006000" "Slate" "#708090")
   (moiety "Moriwen" "Lotus" "#6960EC" "Pomegranate" "#800512")
   (moiety "atheistcanuck" "Plum" "#7D0552" "Redwood" "#A45A52")
   (moiety "lintamande" "Candy" "#FF0000")
   (moiety "Sine Salvatorem" "Aqua" "#00FFFF" #f "" #t)
   (moiety "DeAnno" "Yellow" "#FFFF00" #f "" #t)
   (moiety "Jarnvidr" "Tyrian" "#4F012E" "Ultramarine" "#3F00FF")
   (moiety "Unbitwise" "Spruce" "#8DBCB4" "Lead" "#413C40")))

(define (hue-from rgb)
  (if (equal? rgb "")
      0
      (let* ([rgbytes (hex->bytes (substring rgb 1))]
             [r (/ (first rgbytes) 255)] [g (/ (second rgbytes) 255)] [b (/ (third rgbytes) 255)]
             [maxrgb (max r g b)] [minrgb (min r g b)]
             [hue (if (zero? (- maxrgb minrgb))
                      360
                      (* 60
                         (cond
                           [(equal? r maxrgb) (/ (- g b) (- maxrgb minrgb))]
                           [(equal? g maxrgb) (+ 2 (/ (- b r) (- maxrgb minrgb)))]
                           [else              (+ 4 (/ (- r g) (- maxrgb minrgb)))]) 
                         ))])
        (if (< hue 0) (+ 360 hue) hue))))

(define (lum-from rgb)
  (if (equal? rgb "")
      0
      (let* ([rgbytes (hex->bytes (substring rgb 1))]
             [r (/ (first rgbytes) 255)] [g (/ (second rgbytes) 255)] [b (/ (third rgbytes) 255)]
             [maxrgb (max r g b)] [minrgb (min r g b)])
        (* 100 (/ (+ maxrgb minrgb) 2)))))

(define (rainbow-sort mlist)
  (sort mlist
        (λ (m1 m2)
          (let ([hue1 (hue-from (Moiety-hex1 m1))] [hue2 (hue-from (Moiety-hex1 m2))])
            (if (equal? hue1 hue2)
                (let ([lum1 (lum-from (Moiety-hex1 m1))] [lum2 (lum-from (Moiety-hex1 m2))])
                  (if (equal? lum1 lum2)
                      (let ([hueB1 (hue-from (Moiety-hex2 m1))] [hueB2 (hue-from (Moiety-hex2 m2))])
                        (if (equal? hueB1 hueB2)
                            (< (lum-from (Moiety-hex2 m1)) (lum-from (Moiety-hex2 m2)))
                            (< hueB1 hueB2)))
                      (< lum1 lum2)))
                (< hue1 hue2))))))

(define moiety-rainbow
  (rainbow-sort moiety-list))

(define (print-moiety-list [ml moiety-list])
  (for-each (λ (m) (print-moiety (Moiety-name m) (Moiety-primary m) (Moiety-hex1 m) 
                                 (Moiety-secondary m) (Moiety-hex2 m) (Moiety-spoiler m))) 
            ml))
