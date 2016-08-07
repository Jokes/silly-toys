#lang racket

(define (string->list str)
  (map (λ (n) (string-ref str n)) (build-list (string-length str) identity)))

(define (~remove v ls)
  (if (member v ls)
      (remove v ls)
      (cons #\! ls)))

(define (string- str1 str2)
  (define (sl- sl1 sl2)
    (if (empty? sl2) (apply string sl1)
        (sl- (~remove (first sl2) sl1) (rest sl2))))
  (sl- (string->list str1) (string->list str2)))
