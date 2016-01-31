#lang racket

(define (avg a b)
  (/ (+ a b) 2))

(define (denominator d)
  (/ 1 (gcd 1 d)))

(define (bloodlines a)
  (map
   (λ (b) (cons b (avg a b)))
   (build-list (/ (denominator a) 2)
               (λ (n) (/ n (denominator a))))))

(define (blood-filter a min)
  (filter (λ (p) (equal? (gcd (cdr p) min) min)) (bloodlines a)))
