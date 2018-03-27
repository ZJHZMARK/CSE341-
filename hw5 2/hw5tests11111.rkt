#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (racketlist->mupllist (list (int 1) (int 2) (var "foo")))
                 (apair (int 1) (apair (int 2) (apair (var "foo") (munit))))
                 "racket to mupl")

   (check-equal? (mupllist->racketlist (apair (int 1)
                                              (apair (int 2)
                                                     (apair (var "foo")
                                                            (munit)))))
                                       (list (int 1) (int 2) (var "foo"))
                                       "mupl to racket")

   (check-equal? (eval-exp (mlet "floo" (int 21) (var "floo")))
                 (int 21)
                 "env extend and lookup")
   
   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-equal? (eval-exp (isgreater (int 6) (int 33))) (int 0) "isgreater")

   (check-equal? (eval-exp (ifnz (ismunit (int 123)) (int 88) (int 99)))
                 (int 99)
                 "ifnz")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (eval-exp (ifmunit (munit) (int 123) (int 456)))
                 (int 123)
                 "ifmunit")

   (check-equal? (eval-exp (mlet* (list (cons "a" (int 1))
                                        (cons "b" (int 2)))
                                  (isgreater (var "a") (var "b"))))
                 (int 0)
                 "mlet* and fun")

   (check-equal? (eval-exp (ifeq (int 7) (int 7) (int 111) (int 222)))
                 (int 111)
                 "ifeq")
   
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)