#lang racket

(require "bytes.rkt")
(provide Moiety Moiety->string moiety
         Moiety-name Moiety-primary Moiety-hex1 Moiety-secondary Moiety-hex2
         moiety-rainbow all-colours unused-colours every-colour 
         hue-from lum-from)

(define (print-moiety name primary hex1 [secondary #f] [hex2 ""] [spoiler #f] [spoiler_second 0])
  (when (equal? spoiler_second 0) (set! spoiler_second spoiler))
  (displayln 
   (if secondary
       (string-append
        "[color=" hex1 "]██[/color][color=" hex2 "]█[/color] " 
        (s-open spoiler) "[color=" hex1 "]" name ": [b]" primary "[/b], " hex1 "[/color]"
        (s-close (and spoiler (not spoiler_second)))
        " "
        (s-open (and (not spoiler) spoiler_second))
        "[size=85][color=" hex2 "]([b]" secondary "[/b], " hex2 ")[/color][/size]" (s-close spoiler_second))
       (string-append
        "[color=" hex1 "]███[/color] " 
        (s-open spoiler) "[color=" hex1 "]"name ": [b]" primary "[/b], " hex1 "[/color]" (s-close spoiler)))))

(define (s-open spoiler) (if spoiler "[spoiler]" ""))
(define (s-close spoiler) (if spoiler "[/spoiler]" ""))

(struct Moiety (name primary hex1 secondary hex2 spoiler spoiler_second) #:transparent)
(define (moiety name primary hex1 [secondary #f] [hex2 ""] [spoiler #f] [spoiler_second 0])
  (Moiety name primary (string-upcase hex1) secondary (string-upcase (if secondary hex2 hex1)) spoiler spoiler_second))

(define (Moiety->string m)
  (string-append
   (Moiety-name m) ": " (Moiety-primary m) " (" (Moiety-hex1 m) ")"
   (if (Moiety-secondary m)
       (string-append ", " (Moiety-secondary m) " (" (Moiety-hex2 m) ")")
       "")))

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
   (moiety "Anya" "Storm" "#757ADB" "Silver" "#A0A0A0")
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
   (moiety "Unbitwise" "Spruce" "#8DBCB4" "Lead" "#413C40")
   (moiety "Tricky" "Tangelo" "#ff4500" "Pomegranate" "#800512")
   (moiety "Shirube" "Mint" "#CCFFCC" "Lilac" "#DCD0FF" #t)
   (moiety "Ezra" "Canary" "#FFEF00" "Black" "#000000" #t #f)
   (moiety "Guilty" "Palatinate" "#682860" "Pine" "#01796F")
   (moiety "Kaylin" "Royal" "#1800DB" "Stormcloud" "#AAB7BF")
   (moiety "Wizard" "Heather" "#9778BE")
   (moiety "Transreal Clouden" "Sky" "#B5D4FF" #f "" #t)
   (moiety "Tulip" "Twilight" "#484AA1" "Cinnabar" "#E34234")
   (moiety "ChaosMagic" "Fuchsia" "#C74375" "Gloom" "#545365")
   (moiety "Rotifer" "Carbon" "#564D48" "Morning" "#E17A40")
   (moiety "Tabs" "Wisteria" "#724D79")
   (moiety "Armada" "Ice" "#E6EBF7" "Blood" "#A71012" #t #f)
   (moiety "Tamien" "Leaf" "#366E17" "Loam" "#3D2C0C")
   (moiety "Linky" "Powder" "#91BFFF")
   (moiety "loki-zen" "Sunlight" "#FCF782" "Blood" "#A71012" #t #f)
   (moiety "Root" "Honeydew" "#D1E572" "Watermelon" "#DB518D" #t #f)
   (moiety "Bobby" "Turquoise" "#AFEEEE" "Eggplant" "#3F2B66" #t #f)
   (moiety "XiBeta" "Coral" "#FFE0E0" #f "" #t)
   (moiety "Lielac" "Lilac" "#DCD0FF" "Lime" "#99C534" #t #f)
   (moiety "alexander" "Life" "#6FB939" "Dawn" "#B7E330" #f #t)
   (moiety "Grace" "Malachite" "#057B5A" "Cherry" "#670104")
   (moiety "Ajzira" "Celeste" "#DBDDD3" "Thunder" "#545F8C" #t)
   (moiety "rusalkii" "Pond" "#259A83" "Dusk" "#402989")
   (moiety "strictlyquadrilateral" "Clematis" "#5f2f9d")
   ))

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
                                 (Moiety-secondary m) (Moiety-hex2 m) (Moiety-spoiler m) (Moiety-spoiler_second m)))
            ml))

(define (combine-duplicates mlist)
  (remove-duplicates
   (let ([rmlist (remove-duplicates mlist)])
     (map (λ (m) 
            (let ([m-in (count (λ (n) (equal? (Moiety-primary n) (Moiety-primary m))) rmlist)])
              (if (> m-in 1)
                  (moiety "(primary and secondary)" (Moiety-primary m) (Moiety-hex1 m) #f "" (Moiety-spoiler m))
                  m)))
          rmlist))))

(define all-colours
  (rainbow-sort 
   (combine-duplicates
    (apply append
           (map
            (λ (m)
              (let ([primary-marker "(primary)"] [secondary-marker "(secondary)"])
                (list
                 (moiety primary-marker (Moiety-primary m) (Moiety-hex1 m) #f "" (Moiety-spoiler m)) 
                 (if (Moiety-secondary m)
                     (moiety secondary-marker (Moiety-secondary m) (Moiety-hex2 m) #f "" 
                             (if (equal? (Moiety-spoiler_second m) 0)
                                 (Moiety-spoiler m)
                                 (Moiety-spoiler_second m)))
                     (moiety primary-marker (Moiety-primary m) (Moiety-hex1 m) #f "" (Moiety-spoiler m))))))
            moiety-list)))))

(define unused-colours
  (let ([unused-marker "[i](unused)[/i]"])
    (rainbow-sort
     (list
      (moiety unused-marker "Emerald" "#20603F")
      (moiety unused-marker "Blush" "#CC6F6F")
      (moiety unused-marker "Smoke" "#693834")
      (moiety unused-marker "Haze" "#704C49")
      (moiety unused-marker "Cinnamon" "#C15A39")
      (moiety unused-marker "Mist" "#D4A697")
      (moiety unused-marker "Driftwood" "#766259")
      (moiety unused-marker "Coal" "#372E29")
      (moiety unused-marker "Toffee" "#ECB939")
      (moiety unused-marker "Cream" "#fffdd0" #f "" #t)
      (moiety unused-marker "Seafoam" "#8EDAB2" #f "" #t)
      (moiety unused-marker "Periwinkle" "#9F9FFF")
      (moiety unused-marker "Nightshade" "#7930B5")
      (moiety unused-marker "Mallow" "#D191FF")
      (moiety unused-marker "Petal" "#C38EE6")
      (moiety unused-marker "Mauve" "#E0B0FF" #f "" #t)
      (moiety unused-marker "Amethyst" "#993BD1")
      (moiety unused-marker "Fog" "#A794B2")
      (moiety unused-marker "Eminence" "#6C3082")
      (moiety unused-marker "Blackberry" "#4C2A4F")
      (moiety unused-marker "Byzantium" "#702963")
      (moiety unused-marker "Thistle" "#8F7C8B")
      (moiety unused-marker "Mulberry" "#6E235D")
      (moiety unused-marker "Damask" "#9C4975")
      (moiety unused-marker "Pansy" "#78184A")
      (moiety unused-marker "Platinum" "#C8BECE" #f "" #t)
      (moiety unused-marker "Flint" "#636268")
      (moiety unused-marker "Ashes" "#4B4946")
      (moiety unused-marker "Shadow" "#292B38")
      (moiety unused-marker "Iris" "#525195")
      (moiety unused-marker "Lavender" "#C080FF")
      (moiety unused-marker "Steel" "#556979")
      (moiety unused-marker "Denim" "#2F4557")
      (moiety unused-marker "Jade" "#61AB89")
      (moiety unused-marker "Spearmint" "#148E67")
      (moiety unused-marker "Algae" "#97AF8B")
      (moiety unused-marker "Murk" "#4B4420")
      (moiety unused-marker "Amber" "#C18E1B")
      (moiety unused-marker "Ivory" "#FFD297" #f "" #t)
      (moiety unused-marker "Buttercup" "#F6BF6C")
      (moiety unused-marker "Marigold" "#FFB53C")
      (moiety unused-marker "Cantaloupe" "#FF984F")
      (moiety unused-marker "Terracotta" "#B24407")
      (moiety unused-marker "Caramel" "#C47149")
      (moiety unused-marker "Stone" "#827A64")
      (moiety unused-marker "Garnet" "#581014")
      (moiety unused-marker "Watermelon" "#DB518D")
      (moiety unused-marker "Clay" "#603E3D")
      (moiety unused-marker "Sable" "#57372C")
      (moiety unused-marker "Hickory" "#72573A")
      (moiety unused-marker "Soil" "#5A4534")
      (moiety unused-marker "Raspberry" "#8A024A")
      (moiety unused-marker "Brick" "#9A534D")))))

(define every-colour
  (rainbow-sort (append all-colours unused-colours)))

(define (replace-list-post)
  (displayln "List of colours currently in use:")
  (displayln "[spoiler-box=Colours in use]")
  (print-moiety-list all-colours)
  (displayln "[/spoiler-box]")
  (displayln "List of unused colours that have been unofficially named - you are free to use these names for different colours, or these colours under different names:")
  (displayln "[spoiler-box=Unused colours]")
  (print-moiety-list unused-colours)
  (displayln "[/spoiler-box]")    
  (displayln "Mixed list:")
  (displayln "[spoiler-box=All the colours]")
  (print-moiety-list every-colour)
  (displayln "[/spoiler-box]"))
