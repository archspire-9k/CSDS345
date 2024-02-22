#lang racket

;======================================================
;; CSDS 345 Interpreter Part 1
;; Spring 2024
;; Group 7 
;; Helen Nguyen, Duong Nguyen, Matt Le
;======================================================

; import map functions from mapfunctionhelper
(require "mapfunctionshelper.rkt")

; export
(provide new-state
         (prefix-out state-
                     (combine-out declare
                                  assign
                                  declared?
                                  initialized?
                                  value
                                  return?
                                  set-return-value
                                  get-return-value)))


; instantiate new state
(define new-state '())

(define (initial-state) '((true #t) (false #f)))

(define declare
  (lambda (name state)
    (map-replace name null state)))


(define assign
  (lambda (name value state)
    (map-replace name value state)))


(define declared?
  (lambda (name state)
    (map-containsKey? name state)))


(define initialized?
  (lambda (name state)
    (not (null? value))))


(define value
  (lambda (name state)
    (map-getKey name state)))


(define return-value-name "return-value")


(define return?
  (lambda (state)
    (map-containsKey? return-value-name state)))


(define get-return-value
  (lambda (state)
    (value return-value-name state)))


(define set-return-value
  (lambda (value state)
    (assign return-value-name value state)))