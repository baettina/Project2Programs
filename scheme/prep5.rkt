#lang racket
(define (collatz n)
  (display n)(newline);
  (cond [(eq? n 1) (newline)]
        [(even? n) (collatz (/ n 2))]
        [(odd? n) (collatz (+ (* 3 n) 1))]))