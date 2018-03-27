#lang racket

(require "hw4.rkt")

(require rackunit)

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-k-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dan-then-dog) 27))

(define xxx (list "a" "b" "yes" "no" "DeepDarkFantasy" ""))
(define yyy (list 3 4 5 6))
(define vector1 (vector 1 2 3 (cons 1 3) (cons 2 4) (list 3 4 5)))
(define list1 (list (cons 1 3) (cons 2 4) (cons 3 5) (list 4 5 6)
                    (cons 5 7) (cons 6 8) (list 7 8 9)))

(define ones (lambda () (cons 1 ones)))
(define nats
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


; test 1
(check-equal? (sequence 2 3 11) '(3 5 7 9 11) "Test1.1")
(check-equal? (sequence 3 3 8) '(3 6) "Test1.2")
(check-equal? (sequence 1 3 2) '() "Test1.3")
(check-equal? nums '(0 1 2 3 4 5) "Test1.4")
(println "Tests for function 1 all passed")

; test 2
(check-equal? files '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "Test2.1")
(check-equal? (string-append-map xxx "?!")
              '("a?!" "b?!" "yes?!" "no?!" "DeepDarkFantasy?!" "?!") "Test2.2")
(check-equal? (string-append-map (list "" "" "") "a")
              '("a" "a" "a") "Test2.3")
(println "Tests for function 2 all passed")

; test 3
(check-equal? (list-nth-mod xxx 6) "a" "Test3.1")
(check-equal? (list-nth-mod xxx 8) "yes" "Test3.2")
(check-equal? (list-nth-mod yyy 14) 5 "Test3.3")
(check-exn (regexp "list-nth-mod: negative number")
           (lambda()(list-nth-mod xxx -2)) "Test3.4")
(check-exn (regexp "list-nth-mod: empty list")
           (lambda()(list-nth-mod '() 0)) "Test3.5")
(println "Tests for function 3 all passed")

; test 4~8 & 12
(check-equal? (stream-for-k-steps ones 3) '(1 1 1) "Test4.1")
(check-equal? (stream-for-k-steps ones 0) '() "Test4.2")
(check-equal? (stream-for-k-steps nats 4) '(1 2 3 4) "Test4.3")

(check-equal? (stream-for-k-steps funny-number-stream 13)
              '(1 2 3 4 5 -6 7 8 9 10 11 -12 13) "Test5")
(check-equal? (stream-for-k-steps dan-then-dog 5)
              '("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg") "Test6")
(check-equal? (stream-for-k-steps (stream-add-one funny-number-stream) 6)
              '((1 . 1)(1 . 2)(1 . 3)(1 . 4)(1 . 5)(1 . -6)) "Test7")
(check-equal? (stream-for-k-steps (cycle-lists xxx yyy) 6)
              '(("a" . 3)("b" . 4)("yes" . 5)("no" . 6)
                         ("DeepDarkFantasy" . 3)("" . 4)) "Test8.1")
(check-equal? (stream-for-k-steps (cycle-lists list1 xxx) 4)
              '(((1 . 3) . "a")((2 . 4) . "b")((3 . 5) . "yes")
                               ((4 5 6) . "no")) "Test8.2")
(check-equal? (stream-for-k-steps (cycle-lists-challenge xxx yyy) 6)
              '(("a" . 3)("b" . 4)("yes" . 5)("no" . 6)
                         ("DeepDarkFantasy" . 3)("" . 4)) "Test12.1")
(check-equal? (stream-for-k-steps (cycle-lists-challenge list1 xxx) 4)
              '(((1 . 3) . "a")((2 . 4) . "b")((3 . 5) . "yes")
                               ((4 5 6) . "no")) "Test12.2")
(println "Tests for Question 4~8 & 12 all passed")
              
; test 9
(check-equal? (vector-assoc 1 vector1) (cons 1 3) "Test9.1")
(check-equal? (vector-assoc 2 vector1) (cons 2 4) "Test9.2")
(check-equal? (vector-assoc 3 vector1) '(3 4 5) "Test9.3")
(check-equal? (vector-assoc 4 vector1) #f "Test9.4")
(println "Tests for function 9 all passed")

;; Tests for 12 using example given by HW spec

;; Tests for 10 and 13 depends on printing out cache and priority list
(define test10 (caching-assoc list1 3))

(check-equal? (test10 4) '(4 5 6) "Test10.1")
(check-equal? (test10 7) '(7 8 9) "Test10.2")

(println "Tests for function 10 & 13 all passed")

              











