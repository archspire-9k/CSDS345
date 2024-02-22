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

(provide interpret
         inital_states
          M_statement_list)

;======================================================
; INTERPRETER
;======================================================

; Inteprets the file and passes it to our parseCarOfTree function
(define interpret
  (lambda (filename)
    (generate-parse-tree (parser filename) '( (() ()) ) )))

(define generate-parse-tree
  (lambda (parse-tree state)
    (cond
      ((null? parse-tree) state)
      (else(generate-parse-tree (rest parse-tree) (M_state (first parse-tree) state))))))

;======================================================
; ABSTRACTION
;======================================================
; initial state
(define inital_states
  (lambda (state)
    (cond
      [(eq? #t (state-get-return-value state))         'true]
      [(eq? #f (state-get-return-value state))         'false]
      [(number? (state-get-return-value state))        (state-get-return-value state)]
      [else                                            (error "returned a value, but unsupported type: "
                                                              (state-get-return-value state))])))


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

; M-Value
(define first_symbol car)
(define rest_symbol cdr)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define M_state
  (lambda (statement state)
    (cond
      ((null? statement) (error "statement is null"))
      ;return 
      ((return? statement) (M_return (state-get-return-value statement) state))
      ;var
      ((declaration? statement) (M_declaration statement state))
      ;assignment
      ((assign? statement) (M_assign (leftoperand statement) (rightoperand statement) state))
      ;if
      ((if? statement)    (M_if statement state))
      ;while
      ((while? statement) (M_while statement state))
      (else ("Unable to parse")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT LIST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; executes a list of statements with a given initial state
;; and returns the resulting state
;; error if the statement list terminates w/out return
(define M_statement_list
  (lambda (statement-list state)
    (cond
      [(state-return? state)              state] 
      [(null? statement-list)             (error "null statement, no return statement")]
      [else                               (M_statement_list (cdr statement-list)
                                                   (M_state (car statement-list)
                                                                     state))])))

; Returns the value through value state
(define M_return
  (lambda (statement state)
      (M_value statement state)))

;======================================================
; M_VALUE FUNCTIONS
;======================================================
;;; ; Returns a value using a handler based value type
;;; (define M_value
;;;   (lambda (statement state)
;;;     (cond
;;;       ((number? statement) statement)
;;;       ((var? statement) (M_var_value statement state))
;;;       ((boolean? statement) statement)
;;;       ((isArithmetic? statement) (M_arithmetic_op statement state))
;;;       ((isBoolean? statement state) (M_boolean statement state))
;;;       (else "error, not found"))))

(define nested? list?)
(define assign-var second)
(define assign-expr third)

;; returns the value of an expression, in the context of the given state
(define M_value
  (lambda (expr state)
    (cond
      [(null? expr)                        (error "called Mvalue on a null expression")]
      [(not (nested? expr))                ( M_value_initial expr state)]
      ; else nested expr
      [(assign? expr)                   (M_value (assign-expr expr) state)]
      [(boolean? expr)                  (M_boolean expr state)]
      [(valid_operation expr)                      (M_value_operations expr state)]
      [else                                (error "unreachable in Mvalue")])))

;; returns the value of the token given the state
;; token = 1 | 'x | 'true 
(define M_value_initial
  (lambda (token state)
    (cond
      [(number? token)            token]
      [(eq? 'true token)          #t]
      [(eq? 'false token)         #f]
      [else                       (read_var token state)])))

;; retrieves value of a var from state
;; throws appropriate errors if undeclared or uninitialized
(define read_var
  (lambda (var-symbol state)
    (cond
      [(not (state-declared? var-symbol state))       (error "no declaration found")]
      [(not (state-initialized? var-symbol state))    (error "no initialization found")]
      [else                                               (state-value var-symbol state)])))

;; assuming the atom is an op-symbol, returns the associated function
(define op-of-symbol
  (lambda (op-symbol)
    (if (map-containsKey? op-symbol M_arithmetic_op)
        (map-getKey op-symbol M_arithmetic_op)
        (map-getKey op-symbol M_boolean_op
        (map-getKey op-symbol M_boolean_comparison)))))

;; takes a nested expression containing an op
;; and evaluates it
(define M_value_operations
  (lambda (expr state)
    (M_apply_operations (op-of-symbol (first_symbol expr))
                  (M_list_to_value_map
                   (sort_order_expression (first_symbol expr)
                                                     (rest_symbol expr))
                   state))))


;; takes a list of exprs and maps them to values,
;; propagating the state changes (so that they evaluate correctly)
(define M_list_to_value_map
  (lambda (expr-list state)
    (if (null? expr-list)
        expr-list
        (cons (M_value (car expr-list) state)
              (M_list_to_value_map (cdr expr-list)
                                           (M_expression (car expr-list) state))))))


;; takes an op-symbol and a val-list * already in order of associativity
;; returns the value of the op applied to the list of values
(define M_apply_operations
  (lambda (op val-list)
    (cond
      [(eq? 1 (length val-list))                  (op (first val-list))]
      [(eq? 2 (length val-list))                  (op (first val-list) (second val-list))]
      [else                                       (error op val-list)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STATEMENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHUA DOI ***
;; takes a statement
;; returns the state resulting from evaluating it
(define M_statement
  (lambda (statement state)
    (cond
      [(state-return? state)                state] ; exit early on return
      [(null? statement)                    (error "called Mstate on null statement")]
      [(return? statement)               (M_return statement state)]
      [(while? statement)                (M_while statement state)]
      [(if? statement)                   (M_if statement state)]
      [(assign? statement)               (M_assign statement state)]
      [(declaration? statement)          (M_declaration statement state)]
      [else                                 (error "unrecognized stmt:" statement)])))

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
      [else (state-assign name (M_value (cadr expression) state) (M_expression (car expression) state))])))                                                         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; IF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      [else (M_state (statement1) (M_expression condition state))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WHILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



