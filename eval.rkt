#lang racket

;;;; An evaluator
;;;
;;; This is very trivial
;;;

(require "expand.rkt")

(provide evaluate evaluate-sequence
         empty-environment
         augment-environment augment-environment*
         with-environment define-environment)

;;; Environments are alists
;;;

(define empty-environment '())

(define (value-in-environment env s)
  (let ([found (assq s env)])
    (unless found
      (error 'evaluate "~S is unbound" s))
    (cdr found)))

(define (augment-environment env name value)
  (unless (symbol? name)
    (error 'augment-environment "name not a symbol: ~S" name))
  `((,name . ,value) . ,env))

(define (augment-environment* env . names/values)
  (let ae ([e env]
           [nvs names/values])
    (match nvs
      [(list name value)
       (augment-environment e name value)]
      [(list name value more ...)
       (ae (augment-environment e name value) more)]
      [_
       (error 'augment-environment* "mutant bindings ~S" nvs)])))

(define-syntax with-environment
  (syntax-rules ()
    [(_ ((name from)) forms ...)
     (let ([name from])
       forms ...)]
    [(_ ((name from) (var value) more ...) forms ...)
     (with-environment ((name (augment-environment
                               from 'var (evaluate 'value from)))
                        more ...)
       forms ...)]
    [(_ (name (var value) ...) forms ...)
     (with-environment ((name empty-environment) (var value) ...)
       forms ...)]))

(define-syntax define-environment
  (syntax-rules ()
    [(_ (name from) (var value) ...)
     (define name (with-environment ((e from) (var value) ...) e))]
    [(_ name (var value) ...)
     (define name (with-environment (e (var value) ...) e))]))

(define (literal? thing)
  (or (number? thing)
      (string? thing)
      (boolean? thing)))

;;; Teach the macroexpander about our special forms
;;;
(define-special-pattern (quote thing))
(define-special-pattern (lambda args expr ...))
(define-special-pattern (λ args expr ...))
(define-special-pattern (define thing expr ...))

(define (evaluate form (environment empty-environment))
  (evaluate-in-environment (expand-macros form) environment))

(define (evaluate-sequence forms (environment empty-environment))
  (evaluate-sequence-in-environment (map expand-macros forms) environment))

(define (evaluate-in-environment form env)
  (match form
    [(? literal? l)
     l]
    [(? symbol? s)
     (value-in-environment env s)]
    [(list 'quote thing)
     thing]
    [(list 'quote _ ...)
     (error 'evaluate "mutant quote: ~S" form)]
    [(list (or 'λ 'lambda)
           (list (? symbol? args) ...)
           forms ...)
     (make-function args forms env)]
    [(list (or 'λ 'lambda) _ ...)
     (error 'evaluate "mutant λ: ~S" form)]
    [(list 'if test then else)
     (if (evaluate-in-environment test env)
         (evaluate-in-environment then env)
         (evaluate-in-environment else env))]
    [(list 'if _ ...)
     (error 'evaluate "bad if form: ~S" form)]
    [(list operator forms ...)
     (apply-function (evaluate-in-environment operator env)
                     (map (λ (form)
                            (evaluate-in-environment form env))
                          forms))]
    [else
     (error 'evaluate "what is ~S?" form)]))

(define (evaluate-sequence-in-environment forms env)
  (match forms
    [(list (list 'define (? symbol? name) value) forms ...)
     (evaluate-sequence-in-environment
      forms (augment-environment env (evaluate-in-environment value env)))]
    [(list (and mutant (list 'define _ ...)) _ ...)
     (error 'evaluate-sequence "mutant define: ~S" mutant)]
    [(list form)
     (evaluate-in-environment form env)]
    [(list form forms ...)
     ;; Could skip this or make it an error
     (evaluate-in-environment form env)
     (evaluate-sequence-in-environment forms env)]
    [else
     (error 'evaluate-sequence "bad sequence ~S (empty after define?)" forms)]))

(struct function (formals body environment)
  #:constructor-name make-function)

(define (apply-function function args)
  (if (procedure? function)
      ;; Allow for primitives
      (apply function args)
      (let augment ([formals (function-formals function)]
                    [arguments args]
                    [env (function-environment function)])
        (cond
          [(and (null? formals) (null? arguments))
           (evaluate-sequence-in-environment (function-body function) env)]
          [(or (null? formals) (null? arguments))
           (error 'apply-function "argument mismatch: expected ~S got ~S"
                  (length (function-formals function))
                  (length args))]
          [else
           (augment (rest formals) (rest arguments)
                    (augment-environment env
                                         (first formals)
                                         (first arguments)))]))))
