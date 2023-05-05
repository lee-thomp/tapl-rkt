#lang racket

;;; 6.1.5 Exercise [Recommended, ★★★]:

;;; 1. Define a function removenamesΓ (t) that takes a naming context Γ and
;;;    an ordinary term t (with FV(t) ⊆ dom(Γ)) and yields the corresponding
;;;    nameless term.

;; Bumps all de Bruijn indices in a variable mapping up by 1.
(define (↑ Γ)
  (map (λ (pair) `(,(car pair) . ,(+ 1 (cdr pair)))) Γ))

;; Increment the context with a new mapping.
(define (add-var v Γ)
  (cons `(,v . 0) (↑ Γ)))

;; Converts terms using pattern matching substitution over the expression tree.
(define (remove-names Γ t)
  (match t
    ;; Add bound var to mapping, replace vars in body.
    [`(λ (,x) ,exp)
     `(λ ,(remove-names (add-var x Γ) exp))]

    ;; Preserve environment, rename application.
    [`(,t1 ,t2) `(,(remove-names Γ t1) ,(remove-names Γ t2))]

    ;; Find variable association in mapping.
    [(? symbol?) (cdr (assoc t Γ))]

    ;; Error out on unhandled exp.
    [else (display (format "Unkown exp: ~s~n" t))
      (error "Unknown exp")]))

(provide remove-names)

;;; 2. Define a function restorenamesΓ (t) that takes a nameless term t and a
;;;    naming context Γ and produces an ordinary term. (To do this, you will
;;;    need to “make up” names for the variables bound by abstractions in t.
;;;    You may assume that the names in Γ are pairwise distinct and that the set
;;;    V of variable names is ordered, so that it makes sense to say “choose the
;;;    first variable name that is not already in dom(Γ).”)

;; Swap the car and cdr of a pair.
(define (flip pair)
  `(,(cdr pair) . ,(car pair)))

;; Flip a mapping (association list) so the de Bruijn index can be `assoc`ed.
(define (flip-alist Γ)
  (map flip Γ))

;; Grabs a pair in an alist according to the cdrs of the pairs,
;; - like `assoc` returns #f if no association found.
(define (assoc-cdr v lst)
  (let ([mapping (assoc v (flip-alist lst))])
    (if mapping (flip mapping) #f)))

;; Create a new name for a bound variable that is not already in Γ.
;; - This proc produces symbols in the range [a-z] and skips symbols that are
;;   already present in the given mapping.
(define gen-var
  (letrec ([start (char->integer #\a)] 
           [counter start]
           [limit (char->integer #\z)])

    (define (integer->char-symbol i)
      (string->symbol (string (integer->char i))))
  
    ;; Increments counter, wrapping round after #\z.
    (define (inc-counter!)
      (if (= limit counter) 
          (set! counter start)
          (set! counter (add1 counter))))

    ;; Create new var name, check if present in mapping,
    ;; return if name is not in mapping, else generate next var name.
    (λ (Γ)
      (let ([current-sym (integer->char-symbol counter)])
        (begin (inc-counter!)
               (if (assoc current-sym Γ) (gen-var Γ)
                   current-sym))))))

;; Take a nameless lambda term and recover named variables.
(define (restore-names Γ t)
  (match t
    [(? number?) (let ([mapping (assoc-cdr t Γ)])
                   (if mapping (car mapping) (gen-var Γ)))]

    ['(λ 0)
     (let ([v (gen-var Γ)])
       `(λ (,v) ,v))]

    [`(λ ,exp)
     (let ([v (gen-var Γ)])
       `(λ (,v) ,(restore-names (add-var v Γ) exp)))]

    [`(,t1 ,t2) `(,(restore-names Γ t1) ,(restore-names Γ t2))]

    [else (display (format "Unkown exp: ~s~n" t))
          (error "Unknown exp")]))

(provide restore-names)
