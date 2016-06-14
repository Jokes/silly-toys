#lang racket

(require "bytes.rkt")

(define (print-moiety name primary hex1 [secondary #f] [hex2 ""] [spoiler #f])
  (let 
      ([mainline 
        (if secondary
            (string-append
             "[color=" hex1 "]██[/color][color=" hex2 "]█[/color] "
             "[color=" hex1 "]" name ": [b]" primary "[/b], " hex1 "[/color] "
             "[size=85][color=" hex2 "]([b]" secondary "[/b], " hex2 ")[/color][/size]")
            (string-append
             "[color=" hex1 "]███ " name ": [b]" primary "[/b], " hex1 "[/color]"))])
    (displayln (if spoiler (string-append "[spoiler]" mainline "[/spoiler]") mainline))))

(struct Moiety (name primary hex1 secondary hex2 spoiler) #:transparent)
(define (moiety name primary hex1 [secondary #f] [hex2 ""] [spoiler #f])
  (Moiety name primary hex1 secondary hex2 spoiler))

(define moiety-list
  (list
   (moiety "Alicorn" "Green" "#00BF80" "Cerulean" "#007BA7")
   (moiety "Kappa" "Purple" "#8040BF" "Indigo" "#602080")
   (moiety "Aestrix" "Blue" "#0080FF")
   (moiety "Marri" "Red" "#BF4040")
   (moiety "Benedict" "Grey" "#696969")
   (moiety "Anthusiasm" "Orange" "#FF8000")
   (moiety "PlainDealingVillain" "Tan" "#BF8040")
   (moiety "AndaisQ" "Pink" "#FF4080")
   (moiety "Anya" "Lavender" "#C080FF")
   (moiety "Lambda" "Teal" "#008080" "Brass" "#b5a642")
   (moiety "Kel" "Cyan" "#00CFCF")
   (moiety "Adelene" "Brown" "#62371F")
   (moiety "CuriousDiscoverer" "Silver" "#A0A0A0")
   (moiety "Nemo" "Gold" "#FFBF00")
   (moiety "Link" "Violet" "#7F00FF")
   (moiety "Kuuskytkolme" "Lime" "#99C534")
   (moiety "ErinFlight" "Copper" "#d5732a")
   (moiety "MaggieoftheOwls" "White" "#FFFFFF" #f "" #t)
   (moiety "Rockeye" "Black" "#000000")
   (moiety "Pedro" "Salmon" "#ff5c5c" "Peach" "#FFB25C")
   (moiety "Eva" "Midnight" "#000080")
   (moiety "Teceler" "Forest" "#228B22")
   (moiety "Throne3d" "Carmine" "#960018" "Palatinate" "#682860")
   (moiety "Timepoof" "Rose" "#FF91BF")
   (moiety "RoboticLIN" "Venom" "#728C00")
   (moiety "Diaeresis" "Moss" "#006000")
   (moiety "Moriwen" "Lotus" "#6960EC")
   (moiety "atheistcanuck" "Plum" "#7D0552")
   (moiety "lintamande" "Candy" "#FF0000")
   (moiety "Sine Salvatorem" "Aqua" "#00FFFF" #f "" #t)
   (moiety "DeAnno" "Yellow" "#FFFF00" #f "" #t)
   (moiety "Jarnvidr" "Tyrian" "#4F012E")))

(define (hue-from rgb)
  (let* ([rgbytes (hex->bytes (substring rgb 1))]
         [r (/ (first rgbytes) 255)] [g (/ (second rgbytes) 255)] [b (/ (third rgbytes) 255)]
         [maxrgb (max r g b)] [minrgb (min r g b)]
         [hue (if (zero? (- maxrgb minrgb))
                  365
                  (* 60
                     (cond
                          [(equal? r maxrgb) (/ (- g b) (- maxrgb minrgb))]
                          [(equal? g maxrgb) (+ 2 (/ (- b r) (- maxrgb minrgb)))]
                          [else              (+ 4 (/ (- r g) (- maxrgb minrgb)))]) 
                        ))])
    (if (< hue 0) (+ 360 hue) hue)))

(define moiety-rainbow
  (sort moiety-list
        (λ (m1 m2)
          (< (hue-from (Moiety-hex1 m1)) (hue-from (Moiety-hex1 m2))))))

(define (print-moiety-list [ml moiety-list])
  (for-each (λ (m) (print-moiety (Moiety-name m) (Moiety-primary m) (Moiety-hex1 m) 
                                 (Moiety-secondary m) (Moiety-hex2 m) (Moiety-spoiler m))) 
            ml))