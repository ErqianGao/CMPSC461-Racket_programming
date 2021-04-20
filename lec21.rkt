#lang racket
(define double (lambda (x) (+ x x)))
(double (* 3 4))


(if #t 0 ((lambda (x) (x x)) (lambda (x) (x x))))
;(define (myif b tv fv) (if b tv fv))
;(myif #t 0 ((lambda (x) (x x)) (lambda (x) (x x))))

(define (myif2 b tvthunk fvthunk) (if b (tvthunk) (fvthunk)))

(myif2 #t (lambda () 0) (lambda () ((lambda (x) (x x)) (lambda (x) (x x)))))

(define (slow-add x y)
  (letrec ([slow-id (lambda (m count)
                      (if (= 0 count)
                          m
                          (slow-id m (- count 1))))])
    (+ (slow-id x 50000000) y)))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(define (my-mult2 x y-thunk)
  (let [(y (y-thunk))]
    (cond [(= x 0) 0]
          [(= x 1) y]
          [#t (+ y (my-mult (- x 1) (lambda () y)))])))