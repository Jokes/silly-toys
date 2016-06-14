#lang racket

(provide define-recursing λ-recursing)
(require racket/block)

(define-syntax define-recursing
  (syntax-rules (on with pre)
    [(define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...) (with [«new-param» «default-value»] ...)
       (pre «pre-expr» ...)
       «body-expr»
       ...)
     (define («function-id» «parameter-id» ...)
       «pre-expr» ...
       (define («function-id» «recurse-id» ... [«new-param» «default-value»] ...)
         «body-expr»
         ...)
       («function-id» «recurse-id» ...))]
    
    [(define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...) (with [«new-param» «default-value»] ...)
       «body-expr»
       ...)
     (define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...) (with [«new-param» «default-value»] ...) 
       (pre)
       «body-expr»
       ...)]
    
    [(define-recursing («function-id» «parameter-id» ...) 
       (with [«new-param» «default-value»] ...) (on «recurse-id» ...)
       «body-expr»
       ...)
     (define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...) (with [«new-param» «default-value»] ...) 
       (pre)
       «body-expr»
       ...)]
    
    [(define-recursing («function-id» «parameter-id» ...) 
       (with [«new-param» «default-value»] ...) (on «recurse-id» ...)
       (pre «pre-expr» ...)
       «body-expr»
       ...)
     (define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...) (with [«new-param» «default-value»] ...) 
       (pre «pre-expr» ...)
       «body-expr»
       ...)]
    
    [(define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...)
       «body-expr»
       ...)
     (define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...) (with) 
       (pre)
       «body-expr»
       ...)]
    
    [(define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...)
       (pre «pre-expr» ...)
       «body-expr»
       ...)
     (define-recursing («function-id» «parameter-id» ...) 
       (on «recurse-id» ...) (with) 
       (pre «pre-expr» ...)
       «body-expr»
       ...)]
    
    [(define-recursing («function-id» «parameter-id» ...) 
       (with [«new-param» «default-value»] ...)
       «body-expr»
       ...)
     (define-recursing («function-id» «parameter-id» ...) 
       (on «parameter-id» ...) (with [«new-param» «default-value»] ...) 
       (pre)
       «body-expr»
       ...)]
    
    [(define-recursing («function-id» «parameter-id» ...) 
       (with [«new-param» «default-value»] ...)
       (pre «pre-expr» ...)
       «body-expr»
       ...)
     (define-recursing («function-id» «parameter-id» ...) 
       (on «parameter-id» ...) (with [«new-param» «default-value»] ...) 
       (pre «pre-expr» ...)
       «body-expr»
       ...)]))

(define-syntax-rule (λ-recursing («function-id» «parameter-id» ...) «body-expr» ...)
  (block
   (define («function-id» «parameter-id» ...) «body-expr» ...)
   «function-id»))