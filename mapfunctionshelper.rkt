#lang racket

;======================================================
;; CSDS 345 Interpreter Part 1
;; Spring 2024
;; Group 7 
;; Helen Nguyen, Duong Nguyen, Matt Le
;======================================================

; export functions
(provide (prefix-out map-
                     (combine-out addElement
                                  containsKey?
                                  getKey
                                  firstEncounter
                                  removeall
                                  removefirst
                                  replace
                                  from_interlaced_entry_list)))

; Map Functions

(define empty '())
; add element into map
(define addElement
  (lambda (key element map)
    (cons (cons key element) map)))

; returns whether the entry exists in key 
(define containsKey?
  (lambda (key map)
    (ormap (lambda (entry) (eq? key (car entry))) map)))

; returns the value of the entry in the map-list with the given key if present else null
(define getKey
  (lambda (key map)
    (cond
      [(null? map) null]
      [(eq? key (car (car map))) (cdr (car map))]
      [else (getKey key (cdr map))])))

; returns first encounter value
(define firstEncounter
  (lambda (key begin map)
    (cond
      [(null? map) begin]
      [(eq? key (car (car map))) ((car (car map)))]
      [else (firstEncounter key begin (cdr map))])))

; remove all occurence matching key
; from lecture 
(define removeall
  (lambda (key map)
    (cond
      [(null? map) map]
      [(eq? key (caar map)) (removeall key (cdr map))]
      [else (cons (car map) (removeall key (cdr map)))])))

; remove first occurence matching key
; from lecture
(define removefirst
  (lambda (key map)
    (cond
      ((null? map) map)
      ((eq? key (caar map))(cdr map))
      (else (cons (car map) (removefirst key (cdr map)))))))

; update entries
(define replace
  (lambda (key value map)
    (addElement key value (removeall key map))))

;; treats the first and second elems as key and value
;; returns a map with the entries
(define from_interlaced_entry_list
  (lambda (lis map)
    (if (null? lis)
        map
        (from_interlaced_entry_list(cdr (cdr lis))
                                    (addElement (first lis) (second lis) map)))))



