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
          (car (list-tail xs (remainder (length xs) n)))
          )
      )
)