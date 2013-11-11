#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))
      )
  )

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda(i) (string-append i suffix)) xs)
  )

;; Problem 3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (empty? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
      )
  )

;; Problem 5
(define (funny-generator num)
      (let ([nextnum (if (= (remainder num 5) 0)
                         (- 0 num)
                         num
                         )])
        (lambda () (cons nextnum (funny-generator (+ num 1))))))
(define funny-number-stream
    (funny-generator 1))

;; Problem 6
(define (this-then-other op1 op2 this)
  (if (equal? this op1)
      (lambda () (cons op1 (this-then-other op1 op2 op2)))
      (lambda () (cons op2 (this-then-other op1 op2 op1)))))

(define dan-then-dog
  (this-then-other "dan.jpg" "dog.jpg" "dan.jpg"))

;; Problem 7
(define (stream-add-zero s)
  (lambda () 
    (cons
     (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

;; Problem 8
(define (cycle-lists-helper xs ys n)
  (lambda () 
    (cons
     (cons (list-nth-mod xs n) (list-nth-mod ys n))
     (cycle-lists-helper xs ys (+ n 1)))))

(define (cycle-lists xs ys)
  (cycle-lists-helper xs ys 0))

;; Problem 9
(define (vector-assoc v vec)
  (letrec ([vhelper (lambda (vec i) 
                      (if (< i (vector-length vec))
                          (if (pair? (vector-ref vec i))
                              (if (equal? (car (vector-ref vec i)) v)
                                  (vector-ref vec i)
                                  (vhelper vec (+ i 1)))
                              (vhelper vec (+ i 1)))
                          #f))])
    (vhelper vec 0)))

;; Problem 10
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define index 0)
  (define (get-cache k i)
    (if (or (negative? i) (< i (- index n))) #f
        (let ((cur (vector-ref cache (modulo i n))))
          (if (equal? (car cur) k) cur
              (get-cache k (sub1 i))))))
  (define (update-cache value)
    (vector-set! cache (modulo index n) value)
    (set! index (add1 index))
    value)
  (lambda (k)
    (cond ((get-cache k (sub1 index)))
          ((assoc k xs) => update-cache)
          (else #f))))

;;Problem 11
(define-syntax while-less
  (syntax-rules (do) 
  [(while-less2 e1 do e2) 
   (letrec ([loop (lambda () 
                    (let ([valz2 e2])
                    (if (<= e1 valz2) #t 
                        (loop) )))]) 
     (loop))]))