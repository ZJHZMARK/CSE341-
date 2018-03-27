#lang racket

(require "hw4.rkt") 

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

; 1.
(define test1 (equal? (list 1 4 7 10) (sequence 3 1 11)))
(string-append "test1: " (format "~a" test1))

; 2.
(define test2 (equal? (list "ab" "bb" "cb" "db") (string-append-map (list "a" "b" "c" "d") "b")))
(string-append "test2: " (format "~a" test2))

; 3.
(define test3a (equal? (list-nth-mod (list "a" "b" "c") 5) "c"))
(string-append "test3a: " (format "~a" test3a))
(define test3b (equal? (list-nth-mod (list "a" "b" "c") 0) "a"))
(string-append "test3b: " (format "~a" test3b))

; 4/5.
(define test4/5 (equal? (stream-for-k-steps funny-number-stream 10) (list 1 2 3 4 5 -6 7 8 9 10)))
(string-append "test4/5: " (format "~a" test4/5))

;6.
(define test6 (equal? (stream-for-k-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg")))
(string-append "test6: " (format "~a" test6))

;7.
(define test7 (equal? (stream-for-k-steps (stream-add-one funny-number-stream) 6) (list (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4) (cons 1 5) (cons 1 -6))))
(string-append "test7: " (format "~a" test7))

;8.
(define test8 (equal? (stream-for-k-steps (cycle-lists (list 1 2 3) (list "a" "b")) 5) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a"))))
(string-append "test8: " (format "~a" test8))

;9.
(define lista (list (cons "a" 1) (cons "b" 2) (cons "c" 3) (cons "d" 4)))
(define veca (list->vector lista))
(define test9a (equal? (vector-assoc "e" veca) #f))
(string-append "test9a: " (format "~a" test9a))
(define test9b (equal? (vector-assoc "a" veca) (cons "a" 1)))
(string-append "test9b: " (format "~a" test9b))

;10.
(define listb (list (cons "a" 1) (cons "b" 2) (cons "c" 3) (cons "d" 4)))
(define fn (caching-assoc listb 3))
(define test10a (not (fn "e")))
(string-append "test10a: " (format "~a" test10a))
(define test10b (equal? (fn "a") (cons "a" 1)))
(string-append "test10b: " (format "~a" test10b))
(define test10c (equal? (fn "a") (cons "a" 1)))
(string-append "test10c: " (format "~a" test10c))
(define test10d (equal? (fn "c") (cons "c" 3)))
(string-append "test10d: " (format "~a" test10d))

;11.
(define a 7)
(define counter 0)
(define proc (while-greater 2 do (begin (set! a (- a 1)) (set! counter (+ counter 1)) a)))
(define test11 (and (equal? counter 5) (equal? a 2)))
(string-append "test11: " (format "~a" test11))

;12.
(define test12 (equal? (stream-for-k-steps (cycle-lists-challenge (list 1 2 3) (list "a" "b")) 5) (list (cons 1 "a") (cons 2 "b") (cons 3 "a") (cons 1 "b") (cons 2 "a"))))
(string-append "test12: " (format "~a" test12))

(string-append "All tests passed: " (format "~a" (and test1 test2 test3a test3b test4/5 test6 test7 test8 test9a test9b test10a test10b test10c test10d test11 test12)))
