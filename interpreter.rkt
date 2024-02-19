#lang racket

;======================================================
;; CSDS 345 Interpreter Part 1
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

; Return
(define return-value cadr)

; If
; condition
(define if-condition cadr)
; statement
(define if-statement caddr)

; While
; condition
(define while-condition cadr)
(define while-statement caddr)
  
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
      ((var? lis) (boolean?                        (map-getKey lis state)))
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

;|| -> not tested yet
(define ||?
  (lambda (statement)
    (matches? '|| statement)))

;&& -> not tested yet
(define &&?
  (lambda (statement)
    (matches? '&& statement)))

; Contains valid and defined operation
(define valid_operation
  (lambda (expr)
    (or (map-containsKey? (car expr) M_arithmetic_op)
        (map-containsKey? (car expr)  M_boolean_comparison)
        (map-containsKey? (car expr) M_boolean_op))))

; Returns a value corresponding the var if it exists 
(define M_var_value
  (lambda (var state)
    (cond
      ((state-declared? var state) ;Does this variable exist?
       (cond                     ;If it does exist was it initialized?
         ((false? (state-initialized? var state))      (error "Variable not inititialized"))
         (else                                    (map-getKey var state))))
       (else                                      (error "Variable not Declared")))))

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
      ((eq? (operator lis) '<)                                   (<   (M_boolean_comparison        (leftoperand lis) state) (M_boolean_comparison  (rightoperand lis) state)))
      ((eq? (operator lis) '>)                                  (>   (M_boolean_comparison      (leftoperand lis) state) (M_boolean_comparison  (rightoperand lis) state)))
      ((eq? (operator lis) '<=)                                  (<=  (M_boolean_comparison      (leftoperand lis) state) (M_boolean_comparison   (rightoperand lis) state)))
      ((eq? (operator lis) '>=)                              (>=  (M_boolean_comparison     (leftoperand lis) state) (M_boolean_comparison   (rightoperand lis) state)))
      (((eq? (operator lis) '==))  (eq? (M_value (leftoperand lis) state) (M_value (rightoperand lis) state)))
      (((eq? (operator lis) '!=)) (not (eq? (M_value (leftoperand lis) state) (M_value (rightoperand lis) state))))

      (else (error "Invalid comparison")))))


;======================================================
; M_STATE FUNCTIONS
;======================================================

(define M_state
  (lambda (statement state)
    (cond
      ;return 
      ((return?) (M_return (state-get-return-value statement) state))
      ;var
      ((declaration?) (M_declaration statement state))
      ;assignment
      ((assign?) (M_assign (leftoperand statement) (rightoperand statement) state))
      ;if
      ((if?)    (M_if (if-condition statement) (if-statement statement) state))
      ;while
      ((while?) (M_while (while-condition statement) (while-statement statement) state))
      (else ("Unable to parse")))))

; Returns the value through value state
(define M_return
  (lambda (statement state)
      (M_value statement state)))

; Returns a value using a handler based value type
(define M_value
  (lambda (statement state)
    (cond
      ((number? statement) statement)
      ((var? statement) (M_var_value statement state))
      ((boolean? statement) statement)
      ((isArithmetic? statement) (M_arithmetic_op statement state))
      ((isBoolean? statement state) (M_boolean   statement state))
      (else "error not found"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DECLARATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes a statement representing a declaration statement
;; two cases of simple declaration: var x; and var x = expression: var x = 4

;; takes a declaration statement
;; returns the resulting state
(define M_declaration
  (lambda (statement state)
    (M_declaration_helper (second statement) (cddr statement) state)))

;; declares the variable,
;; error if already declared
;; initializes if expr is provided
(define M_declaration_helper
  (lambda (name expression state)
    (cond
      [(state-declared? name state) (error ("Cannot declare var"))]
      [(null? expression) (state-declare name state)]
      [else (state-assign name (M_value (unbox expression) state) (M_expression (unbox expression) state))])))                                                         
                                                                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; If ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; an if statement
(define if-stmt1 third)
;; return list size 0 or 1
(define if-stmt2 cdddr)

;;returns the state of the if statement
(define M_if
  (lambda (statement state)
    (M_if_helper (if-condition statement) (if-stmt1 statement) (if-stmt2 statement) state)))

(define M_if_helper
  (lambda (condition statement1 statement2 state)
    (cond
      [(M_boolean condition state) (M_state statement1 (M_expression condition state))]
      [(null? statement2) (M_expression condition state)]
      [else (M_state (unbox statement2) (M_expression condition state))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; While ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns the state of the while statement
(define M_while
  (lambda (statement state)
    (M_while_helper (while-condition statement) (while-statement statement) state)))

(define M_while_helper
  (lambda (condition body state)
    (if (M_boolean condition state) 
        (M_while_helper condition body (M_state body (M_expression condition state)))
        (M_expression condition state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Short circuit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns state resulting from performing short-circuit or
(define M_||
  (lambda (expression state)
    (if (M_boolean (first expression) state)
        (M_expression (first expression) state)
        (M_expression (second expression) (M_expression (first expression) state)))))

;; returns state resulting from performing short-circuit and
(define M_&&
  (lambda (expression state)
    (if (M_boolean (first expression) state)
        (M_expression (second expression) (M_expression (first expression) state))
        (M_expression (first expression) state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OP EXPRESSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; takes an expression representing one with an operator
;; for example: !(true || false condition) | a | -a | a + b | a - b
(define op_symbol first)
(define op_param cdr)

(define M_operation
  (lambda (expr state)
    (cond
      [(&&? expr) (M_&& (op_param expr) state)]
      [(||? expr) (M_|| (op_param expr) state)]
      [else (sort_order_expression
            (sort_order_op (op_symbol expr)
            (op_param expr))
                                    state)])))


;; takes an operator and a list of params
;; returns a list of the same params in order
(define sort_order_op
  (lambda (symbol lis) lis))

;; return the state of order expression
(define sort_order_expression
  (lambda (expressions state)
    (if (null? expressions)
        state
        (sort_order_expression (cdr expressions) (M_expression (car expressions) state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPRESSION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a state expression
(define M_expression
  (lambda (expression state)
    (cond
      [(null? expression) (error "null expression")]
      ; base case
      [(not (list? expression)) state]
      ; nested expression
      [(assign? expression) (M_assign expression state)]
      [(valid_operation expression) (M_operation expression state)]
      [else (error "Not found error" expression)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASSIGN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To access the index 
(define second_element second)
(define third_element third)

;; assigns the value resulting from evaluating expr
;; onto the state resulting from evaluating expr
(define M_assign
  (lambda (expression state)
    (M_assign_helper (second_element expression) (third_element expression) state)))

(define M_assign_helper
  (lambda (name expression state)
    (if (state-declared? name state)
        (state-initialized? name
                          (M_value expression state)
                          (M_var_value expression state))
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
      ((isBoolean? condition)     (M_boolean_op condition state))
      (else                          (M_var_value condition state)))))

; Returns a integer value
;(define Minteger
 ; (lambda (expr state)
  ;  (cond
   ; ((number? expr)                        expr)
    ;((var? expr)                           (M_var_value expr state))
    ;((isNeg? expr)                         (-         0                                       (Minteger(leftoperand  expr) state)))
    ;((subtract? expr)                      (-         (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ;((add? expr)                           (+         (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ;((multiply? expr)                      (*         (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ;((divide? expr)                        (quotient  (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ;((remainder? expr)                     (remainder (Minteger(leftoperand expr) state)      (Minteger(rightoperand expr) state)))
    ;(else                                  "No valid operator"))))


