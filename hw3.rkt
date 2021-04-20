#lang racket
(define (double x) (* 2 x)) ; a helper function for testing code.
(define (counter x) (+ x 1))

(define (funpower f n) (lambda (x)
  (if ( = n 0) x
  (if ( = n 1) (f x)
      (if ( > n 0) (f ((funpower f (- n 1)) x)) 0)
  ))))

((funpower sqrt 2) 16) ; should evaluate to 2
((funpower double 0) 3) ; should evaluate to 3

(define (encode n) (lambda (f) (funpower f n)))
(((encode 2) sqrt) 16); should evaluate to 2

(define (decode n)
 ((n counter) 0))
(decode (encode 5)) ; should evaluate to 5