#lang racket

(define (sillyverse s)
  (punctuate (string->list s) (letterverse s)))

(define (punctuate pl ll [sofar '()])
  (if (empty? pl)
      (list->string (reverse sofar))
      (let* ([alph (char-alphabetic? (first pl))]
            [thischr (if alph
                         (if (char-upper-case? (first pl))
                             (char-upcase (first ll))
                             (first ll))
                         (first pl))])
        (punctuate (rest pl) (if alph (rest ll) ll) (cons thischr sofar)))))

(define (letterverse s)
  (map char-downcase (reverse (letters s))))

(define (letters s)
  (filter char-alphabetic? (string->list s)))
