#lang racket/base

(module+ test
  (require rackunit))

(define-syntax define/check
  (syntax-rules (...)
    [(define/check (name arg ...)
       ([in ... out] ...)
       (... ...))
     (begin
       (module+ test
         (fail (format "~a: implementation missing" 'name)))
       (define (name arg ...)
         (cond
           [(and (equal? arg in) ...) out] ...
           [else (error (format "~a: test stub for ~s missing" 'name `(name ,arg ...)))])))]
    [(define/check (name arg ...)
       ([in ... out] ...)
       body0
       body ...)
     (begin
       (module+ test
         (check-equal? (name in ...) out) ...)
       (define (name arg ...)
         body0
         body ...))]))

(define/check (even x)
  (; the tests for even are used as tests
   [3 #f]
   [2 #t]
   [1 #f]
   [0 #t])
  (if (zero? x)
    #t
    (odd (sub1 x))))

(define/check (odd x)
  (; the tests for odd are used to define a test stub
   [3 #t]
   [2 #f]
   [1 #t]
   [0 #f])
  ; the ... triggers the stubbing, but also one error to remind us to eventually implement odd
  ...)
