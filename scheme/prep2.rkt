#lang racket
(define boolist '(#t #f #t #f #f))
(define (truecount lis)
  (length (filter (lambda (x) (eq? x #t)) lis)))