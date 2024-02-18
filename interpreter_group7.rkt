#lang racket

(define interpret
  (lambda (filename)
    (buildParseTree (parser filename) '(() ()))))