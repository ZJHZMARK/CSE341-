
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define vector1 (vector 1 2 3 (cons 1 2) (cons 2 3) (list 3 4 5)))

;; put your code below

;1.take three aregument and print out a sequece of number with spacing value
(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

;2. take a list of string and a string suffix and return a list of string
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;3.take a list and number n and return the mod value of length by n
(define (list-nth-mod xs n)
  (cond((< n 0) (error "list-nth-mod: negative number"))
       ((null? xs) (error "list-nth-mod: empty list"))
       (#t (car (list-tail xs (remainder n (length xs)))))))

;4.return a stream with the first k element value produced
(define (stream-for-k-steps s k)
  (if (<= k 0)
      null
      (cons (car (s)) (stream-for-k-steps (cdr (s)) (- k 1)))))

;5.return a stream of numbers which the number divisible by 6 is negative.
(define funny-number-stream
  (letrec ((f (lambda(x)
                (cons (if (= (remainder x 6) 0) (- x) x)
                      (lambda () (f(+ x 1)))))))
    (lambda () (f 1))))

;6.return a stream which the suequence of it is swtching between dan and dog.
(define dan-then-dog
  (letrec ((f (lambda (x)
                (cons x (lambda() (f (if (eq? x "dan.jpg") "dog.jpg" "dan.jpg")))))))
    (lambda () (f "dan.jpg"))))

;7. take a stream and return a stream which adding 1 to its old content to form pairs.
(define (stream-add-one s)
  (letrec ([f (lambda(x)
                (cons(cons 1 (car(x))) (lambda() (f (cdr (x))))))])
    (lambda () (f s))))

;8. take two list and produce a cycle loop like stream.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f(+ n 1)))))])
    (lambda () (f 0))))

;9. take a value and a vector and return a value that will return a function similar to assoc.
(define (vector-assoc v vec)
  (letrec ([ f (lambda (n)
                 (cond [(<= (vector-length vec) n) #f]
                       [(pair? (vector-ref vec n)) (letrec ([x (vector-ref vec n)])
                                                     (if (equal? v (car x))
                                                         x
                                                         (f (+ n 1))))]
                       [#t (f (+ n 1))]))])
    (f 0)))

;10. take a list and a number n return a assoc funtion that remember the recent actions. 
(define (caching-assoc xs n)
  (letrec([m (make-vector n #f)]
          [count 0])
    (lambda (x)
      (let ([result (vector-assoc x m)])
        (if result
            result
            (let ([nresult (assoc x xs)])
              (if (not nresult)
                  nresult
                  (begin
                    (vector-set! m count nresult)
                    (if (= count( - n 1))
                        (set! count 0)
                        (set! count (+ count 1)))
                    nresult)))))))
  )

;11. make a macro that define a keep evaluate e2 until the value is greater then e1
(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (letrec (
              [result1 e1]
              [f (lambda() (let ([result2 e2])
                   (if (> result2 result1)
                       (f)
                       #t)))])
       (f))]))
       
;12.(challenge) a function similar to cycle-list only it is more efficient. 
(define (cycle-lists-challenge xs ys)
  (letrec ([f (lambda(currentx currenty)
               (cons (cons (car currentx) (car currenty)) (lambda() (letrec ([nxs (cdr currentx)]
                                                                           [nys (cdr currenty)])
                     (f (if (null? nxs) xs nxs) (if (null? nys) ys nys))))))])
    (lambda()(f xs ys))))
                        
                                                                           


          
                                                                           