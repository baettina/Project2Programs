#lang racket
(define pinyin '(ling yi er san si wu liu qi ba jiu shi))
(define english '(zero one two three four five six seven eight nine ten))

(define (member c lis)
  (cond [(null? lis) #f]
        [(eq? c (car lis)) #t]
        [else (member c (cdr lis))]))

(define first (lambda (x) (car x)))
(define second (lambda (x) (cadr x)))
(define rest (lambda (x) (cdr x)))

(define (eng-to-int c)
  (match c
    ['zero 0]
    ['one 1]
    ['two 2]
    ['three 3]
    ['four 4]
    ['five 5]
    ['six 6]
    ['seven 7]
    ['eight 8]
    ['nine 9]
    ['ten 10]
  ))

(define (py-to-int c)
  (match c
    ['ling 0]
    ['yi 1]
    ['er 2]
    ['san 3]
    ['si 4]
    ['wu 5]
    ['liu 6]
    ['qi 7]
    ['ba 8]
    ['jiu 9]
    ['shi 10]
  ))

(define (translate lis)
  (cond [(null? lis) '()]
        [(member (first lis) english) (cons (eng-to-int (first lis))
                                            (translate (rest lis)))]
        [(member (first lis) pinyin) (cons (py-to-int (first lis))
                                           (translate (rest lis)))]
        [else (translate (rest lis))]
  ))

(define (print-perfect lis sep)
  (define n (length lis))
  (define nolast (take lis (- n 1)))
  (define last (drop lis (- n 1)))

  (for-each (lambda (x) (display x) (display sep)) nolast)
  (display (first last))
  )
  
(define (go lis)
  (define intlist (translate lis))
  (display "Translation: ")
      (print-perfect intlist " ")
      (newline);
  (display "Addition: ")
      (print-perfect intlist '+)
      (display " = ")
      (display (foldr + 0 intlist))
      (newline);
  (display "Multiplication: ")
      (print-perfect intlist '*)
      (display " = ")
      (display (foldr * 1 intlist))
      (newline);
)

(define test1 '(yi nine six ba))
(define test2 '(yi josh three si))

(go test1)
(go test2)