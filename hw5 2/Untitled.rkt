#lang racket
(require "hw5.rkt")
(define xx (racketlist->mupllist (list (int 15) (int 3) (int 10))))
(define g12 (eval-exp (call mupl-all-gt (int 9))))
(eval-exp (call g12 xx))