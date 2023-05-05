#lang racket

(require test-engine/racket-tests)

(require "../exercise-6.1.5.rkt")

;;; This pair of functions should have the property that
;;;   removenamesΓ (restorenamesΓ (t)) = t
;;; for any nameless term t, and similarly
;;;   restorenamesΓ (removenamesΓ (t)) = t,
;;; up to renaming of bound variables, for any ordinary term t.

;; Example mapping from 6.1.2.
(define Γ
  '((x . 4)
    (y . 3)
    (z . 2)
    (a . 1)
    (b . 0)))

;; Racket translation of example lambda combinators from 6.1.1 
;; converted to nameless terms using de Bruijn indices.
(define c0*   
  '(λ (λ 0)))

(define c2*
  '(λ (λ (1 (1 0)))))

(define plus*
  '(λ 
     (λ 
       (λ 
         (λ ((3 1) ((2 0) 1)))))))

(define fix*
  '(λ 
     ((λ (1 (λ ((1 1) 0)))) 
      (λ (1 (λ ((1 1) 0)))))))

(define foo*
  '((λ (λ 0)) (λ 0)))

;; Test the behaviour of `remove-names (restore-names)`
(check-expect (remove-names Γ (restore-names Γ c0*))
              c0*)
  
(check-expect (remove-names Γ (restore-names Γ c2*))
              c2*)
  
(check-expect (remove-names Γ (restore-names Γ plus*))
              plus*)
  
(check-expect (remove-names Γ (restore-names Γ fix*))
              fix*)
  
(check-expect (remove-names Γ (restore-names Γ foo*))
              foo*)
  
(define (test-remove-restore-names)
  (test))
(provide test-remove-restore-names)
