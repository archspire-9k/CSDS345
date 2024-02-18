#lang racket
(require "abstractions.rkt")
(require "simpleParser.rkt")
(require "helperFunctions.rkt")
                          


(define interpret
  (lambda (filename)
    (buildParseTree (parser filename) '(() ()))))

; Returns a integer value
(define Minteger
  (lambda (expr state)
    (cond
    ((number? expr)                        expr)
    ((var? expr)                           (M_var_value expr state))
    ((isNeg? expr)                         (-         0                                       (Minteger(leftoperand  expr) state)))
    ((subtract? expr)                      (-         (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ((add? expr)                           (+         (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ((multiply? expr)                      (*         (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ((divide? expr)                        (quotient  (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ((remainder? expr)                     (remainder (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    (else                                  "No valid operator"))))

; Returns a value using a handler based value type
(define M_value
  (lambda (lis state)
    (cond
      ((number? lis)                        lis)
      ((var? lis)              (M_var_value lis state))
      ((boolean? lis)                       lis)
      ((isArithmetic? lis)     (Minteger    lis state))
      ((isBoolean? lis state)  (M_boolean   lis state))
      (else                    "cannot evaluate"
      ))))

; Returns a value corresponding the var if it exists 
(define M_var_value
  (lambda (var state)
    (cond
      ((M_varDeclared var state) ;Does this variable exist?
       (cond                     ;If it does exist was it initialized?
         ((null? (M_lookup var state))      (error "Variable not inititialized"))
         (else                                    (M_lookup var state))))
       (else                                      (error "Variable not Declared")))))

; Categorizes a boolean statement and calls a respective handler if neccessary
; If the statement is an explicit boolean return #t/#f respectively
(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? 'true condition) #t)
      ((eq? 'false condition) #f)
      ((isComparison?       condition)     (M_boolean_comparison condition state))
      ((isBooleanOperation? condition)     (M_boolean_op condition state))
      (else                          (M_var_value condition state)))))



