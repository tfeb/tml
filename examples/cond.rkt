#lang racket

(require tml)

(define primitives
  (augment-environment*
   empty-environment
   'eqv? eqv?
   'void void
   'false #f
   'true #t))

(define-macro (begin . forms)
  `((λ () ,@forms)))

(define-macro (cond . clauses)
  (match clauses
    ['() '(void)]
    [(list (list test forms ...) more ...)
     `(if ,test
          (begin ,@forms)
          (cond ,@more))]
    [_
     (error 'cond "what is ~S?" clauses)]))

(parameterize ([trace-expansion #t])
  (evaluate '(cond ((eqv? 3 4) 5) (false 'false) (true 'true))
            primitives))
     