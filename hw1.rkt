#lang racket
(define (f a b c) (lambda (x) (+ (+ (* a (* x x)) (* b x) c) )))
((f 2 3 4) 0)
((f 2 3 4) 1)
((f 2 3 4) 2)
