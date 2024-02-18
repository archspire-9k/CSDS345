#lang racket
;Abstractions

;Abstractions for variable
(define var?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

; Evaluation abstractions
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)

; Condition abstractions:
(define isArithmetic?
  (lambda (expr)
    (cond
      ((not (list? expr))             #f)
      (else                          (and (not (null? expr)) (member (operator expr) '(+ - * / %)))))))

(define isComparison?
  (lambda (expr)
    (cond
      ((not (list? expr))            #f)
      (else                         (and (not (null? expr)) (member (operator expr) '(< > <= >= == !=)))))))

;is the value a boolean or is the variable is a boolean
(define isBoolean?
  (lambda (lis state)
    (cond
      ((number? lis)                               #f)
      ((boolean? lis)                              #t)
      ((or (isFalse? lis) (isTrue? lis))           #t)
      ((var? lis) (boolean?                        (M_var_value lis state)))
      ((and (not (null? lis)) (and (list? lis)     (member (operator lis) '(== && || > < >= <= ! !=)))))
      (else #f))))
