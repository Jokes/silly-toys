#lang racket

(require "define-recursing.rkt")

(provide hex->bytes bytes->hex
         hex->byte byte->hex
         hex-uppercase)

(define n-zero (char->integer #\0))
(define n-A (char->integer #\A))
(define n-a (char->integer #\a))
(define hex-uppercase #f)

(define-recursing (hex->bytes str) (with [sofar '()])
  (match str
    ["" sofar]
    [(regexp #rx"^0x") (hex->bytes (substring str 2) sofar)]
    [(regexp #rx"^[0-9A-Fa-f][0-9A-Fa-f]")
     (hex->bytes (substring str 2) 
                 (append sofar (list (hex->byte (substring str 0 2)))))]
    [_ sofar]))

(define-recursing (bytes->hex blist) (with [sofar ""])
  (match blist
    ['() sofar]
    [`(,head . ,tail) (bytes->hex tail (string-append sofar (byte->hex head)))]
    [ _ sofar]))

(define (hex->byte str)
  (let ([ls (string->list str)])
    (+ (* 16 (ch->n (first ls)))
       (ch->n (second ls)))))

(define (byte->hex num)
  (string (n->ch (floor (/ num 16)))
          (n->ch (modulo num 16))))

(define (ch->n ch)
  (cond [(char<=? #\0 ch #\9) (- (char->integer ch) n-zero)]
        [(char<=? #\A ch #\F) (+ (- (char->integer ch) n-A) 10)]
        [(char<=? #\a ch #\f) (+ (- (char->integer ch) n-a) 10)]))

(define (n->ch n)
  (cond [(<= 0 n 9) (integer->char (+ n n-zero))]
        [(<= 10 n 15) (integer->char (+ (- n 10) (if hex-uppercase n-A n-a)))]))