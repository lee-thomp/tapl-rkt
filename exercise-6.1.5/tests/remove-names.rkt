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

;; Racket translation of example lambda combinators from 6.1.1.
(define c0   
  '(λ (s) (λ (z) z)))

(define c2   
  '(λ (s) (λ (z) (s (s z)))))

(define plus 
  '(λ (m)
     (λ (n)
       (λ (s)
         (λ (z) ((m s) ((n z) s)))))))

(define fix
  '(λ (f)
     ((λ (x) (f (λ (y) ((x x) y))))
      (λ (x) (f (λ (y) ((x x) y)))))))

(define foo  
  '((λ (x) (λ (x) x)) (λ (x) x)))

;; Test the behaviour of `remove-names`
(check-expect (remove-names Γ c0)
              '(λ (λ 0)))
  
(check-expect (remove-names Γ c2)
              '(λ (λ (1 (1 0)))))
  
(check-expect (remove-names Γ plus)
              '(λ (λ (λ (λ ((3 1) ((2 0) 1)))))))
  
(check-expect (remove-names Γ fix)
              '(λ ((λ (1 (λ ((1 1) 0))))
                   (λ (1 (λ ((1 1) 0)))))))
  
(check-expect (remove-names Γ foo)
              '((λ (λ 0)) (λ 0)))
  
(define (test-remove-names)
  (test))
(provide test-remove-names)
