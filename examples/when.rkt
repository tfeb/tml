#lang racket

(require tml)

(define-macro (begin . forms)
  `((λ () ,@forms)))

(define-macro (when form . forms)
  `(if ,form (begin ,@forms) (void)))

(parameterize ([trace-expansion #t])
  (evaluate '(when 1 2 3)))
