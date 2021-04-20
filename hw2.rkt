#lang racket
(define (my-exp x e)
  (if (= e 0) 1
      (if ( > e 0) (* x (my-exp x (- e 1))) x )
  ))
(my-exp 5 0)
(my-exp 2 3)
(my-exp 123 1)