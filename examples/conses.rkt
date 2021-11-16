#lang racket

(require tml)

(define primitives
  (augment-environment*
   empty-environment
   'eqv? eqv?
   'void void
   'false #f
   'true #t))

(define-environment (conses primitives)
  (cons (λ (a b)
          (λ (m)
            (m a b))))
  (car (λ (c)
         (c (λ (a b) a))))
  (cdr (λ (c)
         (c (λ (a b) b))))
  (nil '())
  (null? (λ (a) (eq? a nil))))

(evaluate '(null? (car (cons 1 2))) conses)