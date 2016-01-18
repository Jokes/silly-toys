#lang racket

(define (% threshold) (< (random 100) threshold))

(define (coin) (< (random 2) 1))
(define \t "	")

(define (cc) (if (coin) 'Corruption 'Crimson))
(define (dn) (if (% 15) 'Dead 'Nobody))

(define (f-grid f n)
  (build-list 
   n
   (λ (a)
     (build-list n (λ (x) (f))))))

(define (print-f-grid f n)
  (for-each
   (λ (ls) (displayln (apply string-append (map (λ (s) (string-append (symbol->string s) \t)) ls))))
   (f-grid f n)))

(define (cc-grid n) (f-grid cc n))
(define (dn-grid n) (f-grid dn n))

(define (all-grid n)
  (apply 
   append
   (build-list 
    n
    (λ (a)
      (list
       (build-list n (λ (x) 'P.))
       (build-list n (λ (x) (cc)))
       (build-list n (λ (x) (dn)))
       (build-list n (λ (x) 'Unexplored)))))))

(define (print-all-grid n)
  (for-each
   (λ (ls) (displayln (apply string-append (map (λ (s) (string-append (symbol->string s) \t)) ls))))
   (all-grid n)))
