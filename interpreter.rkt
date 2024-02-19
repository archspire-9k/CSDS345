#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;IMPORT & EXPORT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require
         "mapfunctionshelper.rkt"
         "simpleParser.rkt"
         )
(require "interpreter_state.rkt")

; *************************** Abtraction ****************************************
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
      ((or (eq? 'true lis) (eq? 'false lis))           #t)
      ((var? lis) (boolean?                        (M_var_value lis state)))
      ((and (not (null? lis)) (and (list? lis)     (member (operator lis) '(== && || > < >= <= ! !=)))))
      (else #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; HELPER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; check whether statements matches symbol
(define matches?
  (lambda (symbol statement)
    (equal? symbol (car statement))))

; get the statement type/condition by checking true or false
; statement from the requirement:
;Formally, a syntax tree is a list where each sublist corresponds
;to a statement. The different statements are:
;variable declaration	(var variable) or (var variable value)
;assignment	(= variable expression)
;return	(return expression)
;if statement	(if conditional then-statement optional-else-statement)
;while statement	(while conditional body-statement)

; var
(define declaration?
  (lambda (statement)
    (matches? 'var statement)))

; assignment
(define assign?
  (lambda (statement)
    (matches? '= statement)))
; return
(define return?
  (lambda (statement)
    (matches? 'return statement)))

;if
(define if?
  (lambda (statement)
    (matches? 'if statement)))

;while
(define while?
  (lambda (statement)
    (matches? 'while statement)))

; arithmetic operations
(define arithmetic_operations
  (map-from-interlaced-entry-list
   (list '+  +
         '-  -
         '/  quotient
         '*  *
         '%  modulo)
   '()))

;Helper functions
; Returns a value corresponding the var if it exists 
(define M_var_value
  (lambda (var state)
    (cond
      ((declared? var state) ;Does this variable exist?
       (cond                     ;If it does exist was it initialized?
         ((false? (intialized? var state))      (error "Variable not inititialized"))
         (else                                    (value var state))))
       (else                                      (error "Variable not Declared")))))


; Processes boolean operations
(define M_boolean_op
  (lambda (lis state)
    (cond
      ((boolean?    lis)     lis)
      ((eq? (operator lis) '!)     (not  (M_boolean (leftoperand lis) state)                                       ))
      ((eq? (operator lis) '||)    (or   (M_boolean (leftoperand lis) state)   (M_boolean (rightoperand lis) state)))
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

; *******************STATES********************
(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? 'true condition) #t)
      ((eq? 'false condition) #f)
      ((isComparison?       condition)     (M_boolean_comparison condition state))
      ((isBooleanOperation? condition)     (M_boolean_op condition state))
      (else                          (M_var_value condition state)))))