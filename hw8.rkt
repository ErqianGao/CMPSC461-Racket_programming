#lang racket
(define (sumsquare n) (mysumsquare n 0))

(define (mysumsquare n sum) (if (= n 0) sum
                              (mysumsquare (- n 1) (+ (* n n) sum))))