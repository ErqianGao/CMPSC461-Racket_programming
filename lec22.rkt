#lang racket

(define (double x) (+ x x))
(define (twice f x) (f (f x)))

(define twice-curry (lambda (f) (lambda (x) (twice f x))))
(define ((twice2 f) x) (f (f x)))

(define (subs a b lst)
  (if (null? lst) null
      (let [(tail (subs a b (cdr lst)))
            (head (car lst))]
        (if (equal? head a)
            (cons b tail)
            (cons head tail)))))

(define (newsubs a b lst)
  (map (lambda (x) (if (equal? a x) b x)) lst))

(define (((curry2 f) x) y) (f x y))
(define ((((curry3 f) x) y) z) (f x y z))

(define ((uncurry2 f) x y) ((f x) y))

(map ((curry2 +) 3) '(1 2 3))

(map (uncurry2 ((curry3 +) 3)) '(1 2 3) '(4 5 6))

((lambda (f) (f 3)) (lambda (x) (cons x '(2))))