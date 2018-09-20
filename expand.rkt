#lang racket

;;;; A trivial CL-style macro-expander for
;;; This is likely to be wrong
;;;

(provide define-macro
         expand
         define-special-pattern)

;;; Operators which have special evaluation rules
;;; (I think there only will be lambda in fact)
;;;

(define special-patterns (make-hasheqv))

(define (special-pattern? op)
  (and (symbol? op)
       (hash-has-key? special-patterns op)))

(define (special-pattern op)
  (hash-ref special-patterns op))

(define-syntax-rule (define-special-pattern (op spec ...))
  (hash-set! special-patterns 'op '(op spec ...)))

(define-special-pattern (lambda args expr ...))


;;; Macros
;;;

(define macros (make-hasheqv))

(define (macro? op)
  (and (symbol? op)
       (hash-has-key? macros op)))

(define (macro op)
  (hash-ref macros op))

(define-syntax-rule (define-macro (m arg ... . tail) form ...)
  (hash-set! macros 'm (lambda (whole)
                         (apply (lambda (arg ... . tail) form ...)
                                (rest whole)))))

(define (expand form)
  ;; expanding a form
  (if (cons? form)
      ;; only compound forms are even considered
      (let ([op (first form)])
        (cond [(macro? op)
               ;; it's a macro: call the macro function & recurse on the result
               (expand ((macro op) form))]
              [(special-pattern? op)
               ;; it's special: use the special expander
               (expand-special form)]
              [else
               ;; just expand every element.
               (map expand form)]))
      form))

(define (expand-special form)
  ;; expand a special thing based on a pattern.
  (match-let* ([(cons op body) form]
               [(cons pop pbody) (special-pattern op)])
    (unless (eqv? op pop)
      (error "bad special pattern" "~s is not ~s" pop op))
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
                                       (expand btf)
                                       btf)
                                   accum)
                             btr ptt ctx))]))))

;;; Sample macros
;;;

(define-macro (let bindings . body)
  `((lambda ,(map first bindings) ,@body)
    ,@(map second bindings)))

(define-macro (when test . forms)
  `(if ,test
       (begin ,@forms)
       (void)))

(define-macro (unless test . forms)
  `(when (not ,test)
     ,@forms))
                            

(define-macro (cond . clauses)
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
;;; Broken
;(define-macro (prog1 form . forms)
;  `(let ([r ,form])
;     ,@forms
;     r))

;;; Working
(define-macro (prog1 form . forms)
  (let ([rn (string->uninterned-symbol "r")])
    `(let ([,rn ,form])
       ,@forms
       ,rn)))
