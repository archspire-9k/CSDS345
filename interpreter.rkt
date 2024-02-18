#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;IMPORT & EXPORT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "interpreter_state.rkt"
         "mapfunctionshelper.rkt"
         "simpleParser.rkt")


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
