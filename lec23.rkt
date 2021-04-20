#lang racket
(define TRUE (lambda (x) (lambda (y) x)))
(define FALSE (lambda (x) (lambda (y) y)))
(define (IF b tv fv) ((b tv) fv))

(define (DECODEBOOL b) ((b #t) #f))

(define (ENCODE n)
  (if (<= n 0)
      (lambda (f) (lambda (x) x))
      (lambda (f) (lambda (x) (f (((ENCODE (- n 1)) f) x))))))

(define ZERO (ENCODE 0))

(define (DECODE n) ((n add1) 0))

(define (SUCC n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (PLUS n1 n2)
  ((n1 SUCC) n2))
;; test PLUS => 7
(DECODE (PLUS (ENCODE 3) (ENCODE 4)))

(define (((curry2 f) x) y) (f x y))

(define (MULT n1 n2)
  ((n1 (lambda (x) (PLUS x n2))) ZERO))



(define (MULT1 n1 n2)
  ((n1 ((curry2 PLUS) n2)) ZERO))
; test MULT => 6
(DECODE (MULT (ENCODE 2) (ENCODE 3)))
; test MULT1 => 12
(DECODE (MULT1 (ENCODE 3) (ENCODE 4)))

(define (PAIR v1 v2) (lambda (f) (f v1 v2)))
(define (LEFT p) (p (lambda (v1 v2) v1)))
(define (RIGHT p) (p (lambda (v1 v2) v2)))


;;; the definition below is not a proper answer to hw7 because it uses DECODE/ENCODE
(define (ISZERO n) (if (zero? (DECODE n)) TRUE FALSE))
(define (PRED n) (ENCODE (- (DECODE n) 1)))
;;;

;; this definition will be an infite recursion because of applicative evaluation of IF function
(define (FACTREC n)
  (IF (ISZERO n)
      (SUCC ZERO)
      (MULT n (FACTREC (PRED n)))))

;; this definition makes each branch for IF a thunk to avoid infinite recursion
(define (FACTREC1 n)
  ((IF (ISZERO n)
      (lambda () (SUCC ZERO))
      (lambda () (MULT n (FACTREC1 (PRED n)))))))

;; test FACTREC1 4 => 24
(DECODE (FACTREC1 (ENCODE 4)))

(define ((FACT0 f) n)
  ((IF (ISZERO n)
      (lambda () (SUCC ZERO))
      (lambda () (MULT n ((f f) (PRED n)))))))
  
(define FACT (FACT0 FACT0))

;; test FACT 5 => 120
(DECODE (FACT (ENCODE 5)))

;; some list functions that work on Church Encoding of List
(define (MAP f lst)
  ((lst (lambda (h t) (lambda (d) (PAIR (f h) (MAP f t))))) FALSE))

(define (FOLDL f init lst)
  ((lst (lambda (h t) (lambda (d) (FOLDL f (f h init) t)))) init))

(define (FOLDR f init lst)
  ((lst (lambda (h t) (lambda (d) (f h (FOLDR f init t))))) init))

;; these are helper functions

;; ENCODELIST takes a list of natural numbers in Racket and
;; converts it to Church Encoded List of Church Numerals.
(define (ENCODELIST lst)
  (foldr (lambda (v l) (PAIR (ENCODE v) l)) FALSE lst))

;; DECODELIST takes Church Encoded List of Church Numerals
;; and converts it to list of natural numbers in Racket.
(define (DECODELIST lst)
  ((lst (lambda (h t) (lambda (d) (cons (DECODE h) (DECODELIST t))))) '()))

;; test MAP SUCC '(1 2 3) => '(2 3 4)
(DECODELIST (MAP SUCC (ENCODELIST '(1 2 3))))

;; test FOLDL MULT (SUCC ZERO) '(2 3 4) => 24
(DECODE (FOLDL MULT (ENCODE 1) (ENCODELIST '(2 3 4))))

;; test FOLDL (lambda (e a) (PAIR (MULT e e) a)) FALSE '(2 3 4)
(DECODELIST (FOLDL (lambda (e a) (PAIR (MULT e e) a)) FALSE (ENCODELIST '(1 2 3))))

;; test FOLDR
(DECODELIST (FOLDR (lambda (e a) (PAIR (PLUS e e) a)) FALSE (ENCODELIST '(1 2 3))))
(define (my-foldl f a lst)
  (if (null? lst) a
      (my-foldl f (f (car lst) a) (cdr lst))))

(define (my-foldr f a lst)
  (if (null? lst) a
      (f (car lst) (my-foldr f a (cdr lst)))))