#lang racket
(define first (lambda (x) (car x)))
(define second (lambda (x) (cadr x)))
(define third (lambda (x) (caddr x)))
(define fourth (lambda (x) (cadddr x)))
(define fifth (lambda (x) (car (rest (rest (rest (rest family)))))))
(define rest (lambda (x) (cdr x)))

(define family '(josh sara erin sandy jon))
(first family)
(second family)
(third family)
(fourth family)
(fifth family)