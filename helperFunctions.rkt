#lang racket
(require "abstractions.rkt")
;Helper functions



; Processes boolean operations
(define M_boolean_op
  (lambda (lis state)
    (cond
      ((boolean?    lis)     lis)
      ((eq? (operator lis) '!)     (not  (M_boolean (leftoperand lis) state)                                       ))
      ((eq? (operator lis) '||)      lis)     (or   (M_boolean (leftoperand lis) state)   (M_boolean (rightoperand lis) state)))
      ((eq? (operator lis) '&&)     (and  (M_boolean (leftoperand lis) state)   (M_boolean (rightoperand lis) state))))))

; Process comparison operations
(define M_boolean_comparison
  (lambda (lis state)
    (cond
      ((boolean? lis)           lis)
      ((eq? (operator lis) '<)                                   (<   (Minteger       (leftoperand lis) state) (Minteger  (rightoperand lis) state)))
      ((eq? (operator lis) '>)                                  (>   (Minteger       (leftoperand lis) state) (Minteger  (rightoperand lis) state)))
      ((eq? (operator lis) '<=)                                  (<=  (Minteger       (leftoperand lis) state) (Minteger  (rightoperand lis) state)))
      ((eq? (operator lis) '>=)                              (>=  (Minteger       (leftoperand lis) state) (Minteger  (rightoperand lis) state)))
      ((and (eq? (operator lis) '==) (areBooleanExpression?    lis state))   (eq? (M_value     (leftoperand lis) state) (M_value (rightoperand lis) state)))
      ((and (eq? (operator lis) '!=) (areBooleanExpression?    lis state))   (not (eq? (M_value (leftoperand lis) state) (M_value (rightoperand lis) state))))

      (else (error "Invalid comparison")))))

