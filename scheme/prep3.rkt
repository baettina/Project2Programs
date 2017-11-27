#lang racket
(define alist '(1 2 3 4 5))
(define (sqrlst lis)
  (map (lambda (x) (* x x)) lis))