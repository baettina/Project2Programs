#lang racket
(define alist '(231 66 101 666 14))
(define (hundreds? lis)
  (filter (lambda (x) (if (> x 100) #t #f)) lis))