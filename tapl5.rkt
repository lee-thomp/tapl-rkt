#lang racket

;; p56 proposes rewriting certain lambda terms with `id` replacing
;; the identity lambda function `(λ (x) x)`
(define (id-rewrite exp)
  (match exp
    [`(λ (,x) ,x) 'id]
    [`(λ (,v) ,t) `(λ (,v) ,(id-rewrite t))]
    [`(,term ,t2)
     `(,(id-rewrite term) ,(id-rewrite t2))]
    [(? symbol?) exp]
    [else (display (format "Unkown exp: ~s~n" exp))
          (error "Unknown exp")]))
