#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "mula.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Project MUL461 Part a"
   ;; tests arithmetic expression
   (check-equal? (eval-exp (mul461-+ (mul461-- (mul461-int 5) (mul461-int 3)) (mul461-* (mul461-int 2) (mul461-int 4)))) (mul461-int 10) "int operation test")

   ;; tests comparison and boolean operation
   (check-equal? (eval-exp (mul461-and
                            (mul461-< (mul461-int 3) (mul461-int 5))
                            (mul461-or (mul461-not (mul461-bool #t))
                                       (mul461-= (mul461-+ (mul461-int 3) (mul461-int 4)) (mul461-int 7)))))
                 (mul461-bool #t) "comparison and boolean operation test")
   ;; tests if
   (check-equal? (eval-exp (mul461-if (mul461-bool #t) (mul461-int 3) (mul461-int 4))) (mul461-int 3) "if true test")
   (check-equal? (eval-exp (mul461-if (mul461-bool #f) (mul461-int 3) (mul461-int 4))) (mul461-int 4) "if true test")
   
   ;; tests let
   (check-equal? (eval-exp (mul461-let "x" (mul461-int 1) (mul461-+ (mul461-int 5) (mul461-var "x")))) (mul461-int 6) "let test")
   
   ;; let* test
   (check-equal? (eval-exp (makelet* (list (cons "x" (mul461-int 10)) (cons "y" (mul461-int 5))) (mul461-- (mul461-var "x") (mul461-var "y")))) (mul461-int 5) "makelet* test")

   ;; factorial test
   (check-equal? (makefactorial 3) (mul461-* (mul461-int 3) (mul461-* (mul461-int 2) (mul461-int 1))) "makefactorial test")
   
))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
