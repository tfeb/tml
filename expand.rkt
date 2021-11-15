#lang racket

;;;; A trivial CL-style macro-expander for fun
;;;
;;; This is likely to be wrong
;;;

(provide define-macro
         expand-macros
         define-special-pattern
         (rename-out (tracing trace-expansion)))

;;; Tracing support
;;;
;;; This perhaps should have an independent existence, which is why
;;; it's in a submodule like this: it's potentially a lot more useful
;;; than the rest of the code.
;;;

(module simple-tracing racket
  (provide tracing trace-output-port
           tracify define/traced)

  (define tracing (make-parameter #f))
  (define trace-output-port (make-parameter (current-error-port)))
  (define trace-level (make-parameter 0))

  (define blanks
    (let ([bhash (make-hasheqv)])
      (λ (n)
        (hash-ref bhash n (thunk (make-string n #\Space))))))

  (define (lprintf fmt . args)
    (display (blanks (trace-level)) (trace-output-port))
    (apply tprintf fmt args))

  (define (tprintf fmt . args)
    (apply fprintf (trace-output-port) fmt args))

  (define (tracify function (name (void)))
    (λ args
      (cond [(tracing)
             (lprintf "[~A" (if (not (void? name)) name "λ"))
             (for ([a (in-list args)])
               (tprintf " ~S" a))
             (tprintf "~%")
             (call-with-values
              (thunk
               (parameterize ([trace-level (+ (trace-level) 1)])
                 (apply function args)))
              (λ vals
                (lprintf " ->")
                (if (null? vals)
                    (tprintf ".")
                    (for ([v (in-list vals)])
                      (tprintf " ~S" v)))
                (tprintf "]~%")
                (apply values vals)))]
            [else
             (apply function args)])))

  (define-syntax define/traced
    (syntax-rules ()
      [(_ (fn . args) form ...)
       (define fn (tracify (λ args form ...) 'fn))]
      [(_ fn function)
       (define fn (tracify function 'fn))])))

(require 'simple-tracing)

;;; Operators which have special evaluation rules
;;;

(define special-patterns (make-hasheqv))

(define (special-pattern? op)
  (and (symbol? op)
       (hash-has-key? special-patterns op)))

(define (special-pattern op)
  (hash-ref special-patterns op))

(define-syntax-rule (define-special-pattern (op spec ...))
  (hash-set! special-patterns 'op '(op spec ...)))

;;; Macros
;;;

(define macros (make-hasheqv))

(define (macro? op)
  (and (symbol? op)
       (hash-has-key? macros op)))

(define (macro op)
  (hash-ref macros op))

(define-syntax-rule (define-macro (m arg ... . tail) form ...)
  (hash-set! macros 'm (λ (whole)
                         (apply (λ (arg ... . tail) form ...)
                                (rest whole)))))

(define/traced (expand-macros form)
  (if (cons? form)
      ;; only compound forms are even considered
      (let ([op (first form)])
        (cond [(macro? op)
               ;; it's a macro: call the macro function & recurse on the result
               (expand-macros ((macro op) form))]
              [(special-pattern? op)
               ;; it's special: use the special expander
               (expand-special form)]
              [else
               ;; just expand every element.
               (map expand-macros form)]))
      form))

(define/traced (expand-special form)
  ;; expand a special thing based on a pattern.
  (match-let* ([(cons op body) form]
               [(cons pop pbody) (special-pattern op)])
    (unless (eqv? op pop)
      (error 'expand-special "~s is not ~s" pop op))
    (let pattern-loop ([accum (list op)]
                       [tail body]
                       [ptail pbody]
                       [context 'expr])
      (cond [(null? tail)
             (unless (or (null? ptail)
                         (eqv? (first ptail) '...))
               (error 'expand-special "~s is not enough forms for ~s"
                      body op))
             (reverse accum)]
            [(null? ptail)
             (error 'expand-special "~s is too many forms for ~s"
                    body op)]
            [else
             (match-let* ([(cons btf btr) tail]
                          [(cons ptf ptr) ptail]
                          [ellipsis? (eqv? ptf '...)]
                          [ctx (if ellipsis? context ptf)]
                          [ptt (if ellipsis? ptail ptr)])
               (pattern-loop (cons (if (eqv? ctx 'expr)
                                       (expand-macros btf)
                                       btf)
                                   accum)
                             btr ptt ctx))]))))


;;; Sample macros
;;;

#||
#;
(hash-set! macros 'let
           ;; this is what define-macro turns into
           (λ (whole)
             (apply (λ (bindings . body)
                      (cons (cons '
                                  (cons (map first bindings) body))
                            (map second bindings)))
                    (rest whole))))

#;
(define-macro (let bindings . body)
  ;; Really primitive version
  (cons (cons 'λ (cons (map first bindings) body))
        (map second bindings)))

#;
(define-macro (let bindings . body)
  ;; without backquote, but usung list* to make it a bit
  ;; less painful
  (list* (list* 'λ (map first bindings) body)
         (map second bindings)))

(define-macro (let bindings . body)
  ;; with backquote
  `((λ ,(map first bindings) ,@body)
    ,@(map second bindings)))

(define-macro (when test . forms)
  ;; just another example
  `(if ,test
       (begin ,@forms)
       (void)))

(define-macro (unless test . forms)
  `(when (not ,test)
     ,@forms))

(define-macro (cond . clauses)
  ;; A more hairy example
  (let expand-cond-clause ([clause (first clauses)]
                           [more-clauses (rest clauses)])
    (match-let ([(cons test forms) clause])
      (case test
        [(else)
         (if (null? more-clauses)
             `(begin ,@forms)
             (error 'cond "clauses after else"))]
        [else
         `(if ,test
              (begin
                ,@forms)
              ,(if (not (null? more-clauses))
                   (expand-cond-clause (first more-clauses)
                                       (rest more-clauses))
                   '(void)))]))))

#;
(define-macro (prog1 form . forms)
  ;; Broken
  `(let ([r ,form])
     ,@forms
     r))

(define-macro (prog1 form . forms)
  ;; Working
  (let ([rn (string->uninterned-symbol "r")])
    `(let ([,rn ,form])
       ,@forms
       ,rn)))
||#
