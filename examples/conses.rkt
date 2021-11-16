#lang racket

(require tml)

(define primitives
  (augment-environment*
   empty-environment
   'eq? eq?
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

(define-macro (begin . forms)
  `((λ () ,@forms)))

(define-macro (when form . forms)
  `(if ,form (begin ,@forms) (void)))

(parameterize ([trace-expansion #t])
  (expand-macros '(when 1 2 3)))

(evaluate '(null? (car (cons 1 2))) conses)