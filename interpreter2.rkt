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
         get_result
         M_state_list)

;======================================================
; ABSTRACT FUNCTIONS
;======================================================

; throw
(define throw cadr)

; M_state_expr
(define nested? list?)

; has_op?
(define action car)

;; maps the corresponding operational symbol
(define operation_symbol
  (lambda (symbol)
    (if (map-containsKey? symbol arithmetic_table)
        (map-getKey symbol arithmetic_table)
        (map-getKey symbol boolean_table))))

; defines all oeprations
(define arithmetic_table
  (map-from_interlaced_entry_list
   (list '+  +
         '-  -
         '/  quotient
         '*  *
         '%  modulo)
   '()))

; defines boolean and comparison operations
(define boolean_table
  (map-from_interlaced_entry_list
   (list '&& (lambda (a b) (and a b))
         '|| (lambda (a b) (or a b))
         '!  not
         '== eq?
         '!= (lambda (a b) (not (eq? a b)))
         '<  <
         '>  >
         '<= <=
         '>= >=)
   '()))

;; check whether operation is contained in tables above
(define contains_op?
  (lambda (expr)
    (or (map-containsKey? (action expr) arithmetic_table)
        (map-containsKey? (action expr) boolean_table))))

;; checks whether the symbol matches with the given symbol
(define matches?
  (lambda (symbl)
    (lambda (statement) (eq? (action statement) symbl))))

;; define for matches
(define assign? (matches? '=))
(define declaration? (matches? 'var))
(define return? (matches? 'return))
(define if? (matches? 'if))
(define while? (matches? 'while))
(define &&? (matches? '&&))
(define ||? (matches? '||))
(define try? (matches? 'try))
(define catch? (matches? 'catch))
(define continue? (matches? 'continue))
(define break? (matches? 'break))
(define throw? (matches? 'throw))
(define finally? (matches? 'finally))
(define begin? (matches? 'begin))

  

;======================================================
; M-VALUE FUNCTION
;======================================================

;; when the atom is not list or var
;; base-case
(define M_value_basic_case
  (lambda (token state)
    (cond
      [(number? token)            token]
      [(eq? 'true token)          #t]
      [(eq? 'false token)         #f]
      [else                       (read-var token state)])))

;; returns state from evaluated expression, given initial state
(define M_expression
  (lambda (expression state)
    (cond
      [(null? expression) (error "Expression not found!")]
      ;not nested
      [(not (nested? expression)) state]   
      ;nested case
      [(assign? expression) (M_assign expression state)]
      [(contains_op? expression) (M_operation expression state)]
      [else error "Expression not found!" expression])))

;======================================================
; M_ASSIGN
;======================================================

(define variable second)
(define assign_expression third)

;; evaluates expression, binds value  to state resulting from the evaluation 
(define M_assign
  (lambda (expression state)
    (M_assign_helper (variable expression) (assign_expression expression) state)))

(define M_assign_helper
  (lambda (name expression state)
    (if (state-declared? name state)
        (state-assign name (M_value expression state) (M_expression expression state))
        (error ("Expression not found!")))))  ;assign to var


;======================================================
; M_OPERATION
;======================================================
(define symbol_for_op first)
(define op-param-list cdr)

;; returns state from processing expression with operator
;; should follow operator's associativity rules
(define M_operation
  (lambda (expr state)
    (cond
      [(||? expr) (M_or (op-param-list expr) state)]
      [(&&? expr) (M_and (op-param-list expr) state)]
      [else (sort_state (sort_op (symbol_for_op expr) (op-param-list expr)) state)])))

;; evaluates expressions in || (boolean logic)
(define M_or
  (lambda (expression state)
    (if (M_boolean (first expression) state)
        (M_expression (first expression) state)
        (M_expression (second expression) (M_expression (first expression) state)))))

;; evaluates expressions in && (boolean logic)
(define M_and
  (lambda (expression state)
    (if (M_boolean (first expression) state)
        (M_expression (second expression) (M_expression (first expression) state))
        (M_expression (first expression) state))))

;; ensures associativity
(define sort_op
  (lambda (op-symbol lis)
    lis)) ; all given ops are left associative


;; sorts the given states
(define sort_state
  (lambda (expr-list state)
    (if (null? expr-list)
        state
        (sort_state (cdr expr-list)
                               (M_expression (car expr-list) state)))))

;======================================================
; M_BOOLEAN
;======================================================
;; Takes a nested boolean expression (with two args)
(define left-bool second)
(define right-bool third)

;; evaluates expression for boolean 
(define M_boolean
  (lambda (expression state)
    (cond
      [(not (nested? expression)) (assert-bool (M_value_basic_case expression state))]
      [(&&? expression)(and (M_boolean (left-bool expression) state) (M_boolean (right-bool expression) (M_expression (left-bool expression) state)))]
      [(||? expression)(or (M_boolean (left-bool expression) state) (M_boolean (right-bool expression) (M_expression (left-bool expression) state)))]
      [else (assert-bool (M_value_operation expression state))])))

; error if not boolean
(define assert-bool
  (lambda (val)
    (if (boolean? val)
        val
        (error "not a boolean"))))


;======================================================
; M_VALUE
;======================================================
;; returns value of expression
(define M_value
  (lambda (expression state)
    (cond
      [(null? expression) (error "Null expression")]
      [(not (nested? expression)) (M_value_basic_case expression state)]
      
      ; nested expression
      [(boolean? expression) (M_boolean expression state)]
      [(assign? expression) (M_value (assign_expression expression) state)]
      [(contains_op? expression) (M_value_operation expression state)]
      [else (error "error in reaching value")])))

;; helper
(define read-var
  (lambda (var-symbol state)
    (cond
      [(not (state-declared? var-symbol state)) (error ("Error declaration"))]
      [(not (state-initialized? var-symbol state)) (error ("Error initialization"))]
      [else (state-value var-symbol state)])))

;; deal with nested lists
(define M_value_operation
  (lambda (expression state)
    (operation_app (operation_symbol (symbol_for_op expression))(expression_to_val (sort_op (symbol_for_op expression) (op-param-list expression)) state))))

;; helper
(define expression_to_val
  (lambda (expr-list state)
    (if (null? expr-list)
        expr-list
        (cons (M_value (car expr-list) state)
              (expression_to_val (cdr expr-list)
                                           (M_expression (car expr-list) state))))))

;; apply the operators
(define operation_app
  (lambda (operation list)
    (cond
      [(eq? 1 (length list)) (operation (first list))]
      [(eq? 2 (length list)) (operation (first list) (second list))]
      [else (error operation list)])))

;======================================================
; M_STATEMENTS
;======================================================
(define M_statement
  (lambda (statement state return continue break throw)
    (cond
      [(state-return? state) state] ; exit
      [(null? statement) (error "null statement")]
      [(return? statement) (M_return statement state)]
      [(while? statement) (M_while statement state return continue break throw)]
      [(if? statement) (M_if statement state return continue break throw)]
      [(assign? statement) (M_assign statement state)]
      [(declaration? statement) (M_declaration statement state)]
      [(begin? statement) (M_block statement state return continue break throw)]
      [(continue? statement) (continue state)]
      [(break? statement) (break (pop state))]
      [(throw? statement) (throw (addElement 'exception (M_value (throw statement) state) state))]
      [(try? statement) (M_try statement state return continue break throw)]
      [(catch? statement) (M_catch statement state return continue break throw) ]
      [(finally? statement) (M_finally statement state return continue break throw)]
      [else (error "error statement" statement)])))

;======================================================
; M_STATE RETURN
;======================================================
(define M_return
  (lambda (statement state)
    (state-set-return-value (M_value (get_expression statement) state) (M_expression (get_expression statement) state))))

;======================================================
; M_STATE WHILE
;======================================================
(define while-condition second)
(define while-statement third)

;; while statement
(define M_while
  (lambda (statement state return continue break throw)
    (call/cc
     (lambda (break)
       (M_while_helper (while-condition statement)
                       (while-statement statement)
                       state return continue break throw)))))

(define M_while_helper
  (lambda (condition body state return continue break throw)
    (if (M_boolean condition state)
        (M_while_helper condition body (M_statement body (call/cc (lambda (continue) (M_statement condition state return continue break throw)))) return continue break throw)
        (M_expression condition state))))

;======================================================
; M_STATE IF
;======================================================
(define condition_ second)
(define statement_if third)
(define statement_if_2 cdddr)

;; if statement
(define M_if
  (lambda (statement state return continue break throw)
    (M_if_helper (condition_ statement) (statement_if statement) (statement_if_2 statement) state return continue break throw)))

(define M_if_helper
  (lambda (condition statement1 statement2 state return continue break throw)
    (cond
      [(M_boolean condition state)(M_statement statement1 (M_expression condition state) return continue break throw)]
      [(null? statement2) (M_expression condition state)]
      [else (M_statement (car statement2) (M_expression condition state) return continue break throw)])))

;======================================================
; M_STATE BLOCK
;======================================================
(define block_body cdr)
(define M_block
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) (pop state))
      ((begin? statement) (M_block (block_body statement) (push state) return continue break throw))
      (else (M_block (block_body statement) (M_statement (car statement) state return continue break throw) return continue break throw))))) ;rename later

;======================================================
; M_STATE TRY
;======================================================
(define try-body cadr)

(define M_try_helper
  (lambda (try statement state return continue break throw)
    (cond
      ((null? statement) state)
      ((eq? 'return (function (first statement))) (M_state (first statement) (M_state-finally (finallyBlock try) state return continue break throw) return continue break throw))
      ((eq? 'break (function (first statement))) (M_state (first statement) (M_state-finally (finallyBlock try) state return continue break throw) return continue break throw))
      (else (M_try_helper try (rest statement) (M_state (first statement) state return continue break throw) return continue break throw)))))

(define M_try
  (lambda (statement state return continue break throw)
    (cond
      ((and (null? (catch_block statement)) (null? (finally_block statement))) (error 'error "nothing in catch and finally block"))
      (else (M_finally (finally_block statement) (M_catch (catchBlock statement) (call/cc (lambda (throw) (M_try_helper statement (try-body statement) state return continue break throw))) return continue break throw) return continue break throw)))))

;======================================================
; M_STATE CATCH
;======================================================
(define catch-statement caddr)

(define get-exception caadr)

(define catch_block
  (lambda (statement)
    (if (null? (caddr statement))
        '()
        (caddr statement))))

(define M_catch
  (lambda (statement state return continue break throw)
       (cond
         ((null? statement) state)
         ((and (catch? statement) (declared? 'exception state)) (M_catch (catch-statement statement) (rename (get-exception statement) state) return continue break throw))
         ((catch? statement) state)
         (else (M_catch (cdr statement) (M_statement (car statement) state return continue break throw) return continue break throw)))))

;======================================================
; M_STATE FINALLY
;======================================================
(define finally_block
  (lambda (statement)
    (if (null? (cadddr statement))
      '()
      (cadr (cadddr statement)))))

(define M_finally
  (lambda (statement state return continue break throw)
    (cond
      ((null? statement) state)
      (else (M_finally (cdr statement) (M_statement (car statement) state return continue break throw) return continue break throw)))))

;======================================================
; M_STATE DECLARE
;======================================================

(define dec_var second)
(define dec_expr cddr)

;; return from a declare statement
(define M_declaration
  (lambda (statement state)
    (M_state_declaration_helper (dec_var statement)
                      (dec_expr statement)
                      state)))

;; declares and check if already declared, then throw error
;; initializes expression
(define M_state_declaration_helper
  (lambda (name expression state)
    (cond
      [(state-declared? name state) (error ("Failed to declare"))]
      [(null? expression) (state-declare name state)]
      [else (state-assign name (M_value (car expression) state) (M_expression (car expression) state))])))


;======================================================
; M_STATE RETURN
;======================================================
(define get_expression second)

;; M_statement_list
(define M_state_list
  (lambda (list state)
    (cond
      [(state-return? state) state] 
      [(null? list) (error "program ended without reaching a return statement")]
      [else (M_state_list (cdr list) (M_statement (car list) state))])))
;======================================================
; INTERPRET & EXECUTION
;======================================================
;; parses the file and returns the correct results
(define interpret
  (lambda (file-name)
    (get_result (M_state_list (parser file-name) new-state))))

;; get result out and binding
(define get_result
  (lambda (state)
    (cond
      [(eq? #f (state-get-return-value state)) 'false]
      [(eq? #t (state-get-return-value state)) 'true]
      [(number? (state-get-return-value state)) (state-get-return-value state)]
      [else (error "Cannot return value")])))

(define interpreterer
  (lambda (expr state)
    (if (null? (cdr expr)) (M_statement (car expr) state)
        (interpreterer (cdr expr) (M_statement (car expr) state)))))
                       
(interpret "test.txt")