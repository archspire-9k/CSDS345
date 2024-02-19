#lang racket

;======================================================
;; CSDS 343 Interpreter Part 1
;; Spring 2024
;; Group 7
;; Helen Nguyen, Duong Nguyen, Matt Le
;======================================================

;======================================================
; IMPORT & EXPORT 
;======================================================
(require
         "mapfunctionshelper.rkt"
         "simpleParser.rkt"
         )
(require "interpreter_state.rkt")

;======================================================
; ABSTRACTION
;======================================================

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


;======================================================
; HELPER FUNCTIONS
;======================================================

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

; Contains valid and defined operation
(define valid_operation
  (lambda (expr)
    (or (map-containsKey? (car expr) M_arithmetic_op)
        (map-containsKey? (car expr)  M_boolean_comparison)
        (map-containsKey? (car expr) M_boolean_op))))

;Helper functions
; Returns a value corresponding the var if it exists 
(define M_var_value
  (lambda (var state)
    (cond
      ((state-declare var state) ;Does this variable exist?
       (cond                     ;If it does exist was it initialized?
         ((false? (state-initialized? var state))      (error "Variable not inititialized"))
         (else                                    (M_var_value var state))))
       (else                                      (error "Variable not Declared")))))

; Helper function
; Given
(define M_value
  (lambda (lis state)
    (cond
      ((null? lis) (error "lis is null"))
      ((number? lis)                        lis)
      ((var? lis)              (M_var_value lis state))
      ((boolean? lis)                       lis)
      ((isArithmetic? lis)     (Minteger    lis state))
      ((isBoolean? lis state)  (M_boolean   lis state))
      (else                    "cannot evaluate"))))

; Processes arithmetic operations
(define M_arithmetic_op
  (lambda (lis state)
    (cond
      ((integer? lis) lis)
      ((var? lis) (state-get-return-value state))
      ((eq? (operator lis) '+) (+ (M_value (leftoperand lis) state) (M_value (rightoperand lis) state)))
      ((eq? (operator lis) '-) (- (M_value (leftoperand lis) state) (M_value (rightoperand lis) state)))    
      ((eq? (operator lis) '*) (* (M_value (leftoperand lis) state) (M_value (rightoperand lis) state)))          
      ((eq? (operator lis) '/) (quotient (M_value (leftoperand lis) state) (M_value (rightoperand lis) state)))            
      ((eq? (operator lis) '%) (modulo (M_value (leftoperand lis) state) (M_value (rightoperand lis) state)))     
      (else (error "Invalid operator")))))

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
      (((eq? (operator lis) '==))  (eq? (M_value (leftoperand lis) state) (M_value (rightoperand lis) state)))
      (((eq? (operator lis) '!=)) (not (eq? (M_value (leftoperand lis) state) (M_value (rightoperand lis) state))))

      (else (error "Invalid comparison")))))

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


;======================================================
; M_STATE FUNCTIONS
;======================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a list representing a declaration statement
; x = expr
(define assign-var second)
(define assign-expr third)

;; assigns the value resulting from evaluating expr
;;  onto the state resulting from evaluating expr
(define Mstate-assign
  (lambda (expr state)
    (Mstate-assign-helper (assign-var expr)
                        (assign-expr expr)
                        state)))

(define Mstate-assign-helper
  (lambda (var-name val-expr state)
    (if (state-declared? var-name state)
        (state-initialized? var-name
                          (Mvalue val-expr state)
                          (M_var_value val-expr state))
        (error ("Have not declared variable")))))

;======================================================
; M_boolean FUNCTIONS
;======================================================
(define M_boolean
  (lambda (condition state)
    (cond
      ((eq? 'true condition) #t)
      ((eq? 'false condition) #f)
      ((isComparison?       condition)     (M_boolean_comparison condition state))
      ((isBooleanOperation? condition)     (M_boolean_op condition state))
      (else                          (M_var_value condition state)))))


